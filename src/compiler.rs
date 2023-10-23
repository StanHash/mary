use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;

use thiserror::Error;

use crate::ast::{AssignOperation, ConstAccess, ConstRef, ConstVal, Expr, Stmt, SwitchCase};
use crate::ir::{CallId, CaseEnum, Ins, IntValue, JumpId, Script, StrValue, SwitchId, VarId};

pub enum NameRef<'a> {
    Const(&'a ConstVal),
    Func(CallId),
    Proc(CallId),
    Var(VarId),
}

pub trait NameAccess {
    fn lookup_name(&self, name: &str) -> Option<NameRef>;
}

impl<N: NameAccess> ConstAccess for N {
    fn lookup_const(&self, name: &str) -> Option<ConstRef> {
        match self.lookup_name(name) {
            Some(NameRef::Const(ConstVal::Int(i))) => Some(ConstRef::Int(*i)),
            Some(NameRef::Const(ConstVal::Str(s))) => Some(ConstRef::Str(s)),
            _ => None,
        }
    }
}

pub struct ConstScope {
    names: HashMap<String, ConstVal>,
    funcs: HashMap<String, (CallId, bool)>,
}

impl ConstScope {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn add_const(&mut self, name: String, val: ConstVal) {
        self.names.insert(name, val);
    }

    pub fn add_func(&mut self, name: String, id: CallId) {
        self.funcs.insert(name, (id, true));
    }

    pub fn add_proc(&mut self, name: String, id: CallId) {
        self.funcs.insert(name, (id, false));
    }
}

impl NameAccess for ConstScope {
    fn lookup_name(&self, name: &str) -> Option<NameRef> {
        if let Some(val) = self.names.get(name) {
            Some(NameRef::Const(val))
        } else {
            match self.funcs.get(name) {
                Some((id, true)) => Some(NameRef::Func(*id)),
                Some((id, false)) => Some(NameRef::Proc(*id)),
                None => None,
            }
        }
    }
}

pub enum NameVal {
    Var(VarId),
    Const(ConstVal),
}

impl<'a> From<&'a NameVal> for NameRef<'a> {
    fn from(value: &'a NameVal) -> Self {
        match value {
            NameVal::Var(id) => Self::Var(*id),
            NameVal::Const(cv) => Self::Const(cv),
        }
    }
}

struct BlockScope<'a> {
    var_frame: usize,
    names: HashMap<String, NameVal>,
    parent: &'a dyn NameAccess,
}

impl<'a> BlockScope<'a> {
    fn new(var_frame: usize, parent: &'a dyn NameAccess) -> Self {
        Self {
            var_frame,
            names: HashMap::new(),
            parent,
        }
    }

    fn next_id(&self) -> usize {
        self.var_frame
    }

    fn define_var(&mut self, name: String) -> Option<VarId> {
        let id = VarId(self.next_id());

        match self.names.entry(name) {
            Entry::Occupied(_) => None,
            Entry::Vacant(v) => {
                self.var_frame = self.var_frame + 1;
                v.insert(NameVal::Var(id));

                Some(id)
            }
        }
    }

    pub fn define_const(&mut self, name: String, val: ConstVal) -> Option<()> {
        match self.names.entry(name) {
            Entry::Occupied(_) => None,
            Entry::Vacant(v) => {
                v.insert(NameVal::Const(val));

                Some(())
            }
        }
    }
}

impl<'a> NameAccess for BlockScope<'a> {
    fn lookup_name(&self, name: &str) -> Option<NameRef> {
        if let Some(nv) = self.names.get(name) {
            Some(nv.into())
        } else {
            self.parent.lookup_name(name)
        }
    }
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Cannot redeclare '{0}' in this scope")]
    NameAlreadyDeclared(String),

    #[error("Cannot assign value to '{0}' (not a variable)")]
    CannotAssignToNonVar(String),

    #[error("Cannot evaluate {0} '{1}'")]
    CannotEvaluateCallable(&'static str, String),

    #[error("Failed to evaluate constant expression")]
    FailedConstantEvaluation,

    #[error("Expected constant integer but got string instead")]
    ExpectedConstantIntGotStr,

    #[error("Name '{0}' is not declared in this scope")]
    NameNotDeclared(String),

    #[error("Cannot call '{0}': only functions and procedures can be called")]
    NotCallable(String),

    #[error("Cannot evaluate call to '{0}': procedures do not yield results")]
    ProcNotFunction(String),

    #[error("Multiple 'default' blocks in switch")]
    MultipleDefaults,
}

use CompileError::*;

#[derive(Debug, Error)]
pub struct CompileErrors(pub Vec<CompileError>);

impl fmt::Display for CompileErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for e in &self.0 {
            write!(f, "{}", e)?;
        }

        Ok(())
    }
}

struct Emit {
    vec: Vec<Ins>,
    lab: usize,
    str: Vec<StrValue>,
    err: Vec<CompileError>,
}

impl Emit {
    fn new() -> Self {
        Self {
            vec: Vec::new(),
            lab: 0,
            str: Vec::new(),
            err: Vec::new(),
        }
    }

    fn new_label(&mut self) -> JumpId {
        self.lab = self.lab + 1;
        JumpId(self.lab)
    }

    fn new_switch(&mut self) -> SwitchId {
        self.lab = self.lab + 1;
        SwitchId(self.lab)
    }

    fn ins(&mut self, ins: Ins) {
        self.vec.push(ins);
    }

    fn str_id(&mut self, string: StrValue) -> IntValue {
        if let Some(i) = self.str.iter().position(|s| s.eq(&string)) {
            i as IntValue
        } else {
            self.str.push(string);
            (self.str.len() - 1) as IntValue
        }
    }

    fn const_value(&mut self, val: &ConstVal) {
        match val {
            ConstVal::Int(value) => self.ins(Ins::PushInt(*value)),
            ConstVal::Str(value) => {
                let id = self.str_id(value.clone());
                self.ins(Ins::PushInt(id))
            }
        }
    }

    fn expr_cmp<B: Fn(JumpId) -> Ins>(&mut self, scope: &BlockScope, lhs: Expr, rhs: Expr, b: B) {
        let true_label = self.new_label();
        let next_label = self.new_label();

        self.expr(scope, lhs);
        self.expr(scope, rhs);
        self.ins(Ins::Cmp);
        self.ins(b(true_label));
        self.ins(Ins::PushInt(0));
        self.ins(Ins::Jmp(next_label));
        self.ins(Ins::Label(true_label));
        self.ins(Ins::PushInt(1));
        self.ins(Ins::Label(next_label));
    }

    fn expr(&mut self, scope: &BlockScope, expr: Expr) {
        match expr {
            Expr::Name(name) => match scope.lookup_name(&name) {
                Some(NameRef::Var(id)) => self.ins(Ins::PushVar(id)),
                Some(NameRef::Const(val)) => self.const_value(val),
                Some(NameRef::Func(_)) => self.err.push(CannotEvaluateCallable("function", name)),
                Some(NameRef::Proc(_)) => self.err.push(CannotEvaluateCallable("procedure", name)),
                None => self.err.push(NameNotDeclared(name)),
            },

            Expr::Int(value) => self.ins(Ins::PushInt(value)),
            Expr::Str(value) => {
                let id = self.str_id(value);
                self.ins(Ins::PushInt(id))
            }

            Expr::OpAdd(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::Add);
            }

            Expr::OpSub(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::Sub);
            }

            Expr::OpMul(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::Mul);
            }

            Expr::OpDiv(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::Div);
            }

            Expr::OpMod(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::Mod);
            }

            Expr::OpOr(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::LogicalOr);
            }

            Expr::OpAnd(ops) => {
                let (lhs, rhs) = *ops;

                self.expr(scope, lhs);
                self.expr(scope, rhs);
                self.ins(Ins::LogicalAnd);
            }

            Expr::OpNeg(inner) => {
                self.expr(scope, *inner);
                self.ins(Ins::Neg);
            }

            Expr::OpNot(inner) => {
                self.expr(scope, *inner);
                self.ins(Ins::LogicalNot);
            }

            Expr::PostIncrement(name) => match scope.lookup_name(&name) {
                Some(NameRef::Var(id)) => {
                    self.ins(Ins::PushVar(id));
                    self.ins(Ins::Dupe);
                    self.ins(Ins::Inc);
                    self.ins(Ins::PopVar(id));
                }

                Some(_) => self.err.push(CannotAssignToNonVar(name)),
                None => self.err.push(NameNotDeclared(name)),
            },

            Expr::PreIncrement(name) => match scope.lookup_name(&name) {
                Some(NameRef::Var(id)) => {
                    self.ins(Ins::PushVar(id));
                    self.ins(Ins::Inc);
                    self.ins(Ins::Dupe);
                    self.ins(Ins::PopVar(id));
                }

                Some(_) => self.err.push(CannotAssignToNonVar(name)),
                None => self.err.push(NameNotDeclared(name)),
            },

            Expr::PostDecrement(name) => match scope.lookup_name(&name) {
                Some(NameRef::Var(id)) => {
                    self.ins(Ins::PushVar(id));
                    self.ins(Ins::Dupe);
                    self.ins(Ins::Dec);
                    self.ins(Ins::PopVar(id));
                }

                Some(_) => self.err.push(CannotAssignToNonVar(name)),
                None => self.err.push(NameNotDeclared(name)),
            },

            Expr::PreDecrement(name) => match scope.lookup_name(&name) {
                Some(NameRef::Var(id)) => {
                    self.ins(Ins::PushVar(id));
                    self.ins(Ins::Dec);
                    self.ins(Ins::Dupe);
                    self.ins(Ins::PopVar(id));
                }

                Some(_) => self.err.push(CannotAssignToNonVar(name)),
                None => self.err.push(NameNotDeclared(name)),
            },

            Expr::CmpEq(ops) => {
                let (lhs, rhs) = *ops;
                self.expr_cmp(scope, lhs, rhs, |id| Ins::Beq(id));
            }

            Expr::CmpNe(ops) => {
                let (lhs, rhs) = *ops;
                self.expr_cmp(scope, lhs, rhs, |id| Ins::Bne(id));
            }

            Expr::CmpLt(ops) => {
                let (lhs, rhs) = *ops;
                self.expr_cmp(scope, lhs, rhs, |id| Ins::Blt(id));
            }

            Expr::CmpLe(ops) => {
                let (lhs, rhs) = *ops;
                self.expr_cmp(scope, lhs, rhs, |id| Ins::Ble(id));
            }

            Expr::CmpGe(ops) => {
                let (lhs, rhs) = *ops;
                self.expr_cmp(scope, lhs, rhs, |id| Ins::Bge(id));
            }

            Expr::CmpGt(ops) => {
                let (lhs, rhs) = *ops;
                self.expr_cmp(scope, lhs, rhs, |id| Ins::Bgt(id));
            }

            Expr::Call(invoke) => match scope.lookup_name(&invoke.func) {
                Some(NameRef::Func(id)) => {
                    for arg in invoke.args {
                        self.expr(scope, arg);
                    }

                    self.ins(Ins::Call(id));
                }

                Some(NameRef::Proc(_)) => self.err.push(ProcNotFunction(invoke.func)),
                Some(_) => self.err.push(NotCallable(invoke.func)),
                None => self.err.push(NameNotDeclared(invoke.func)),
            },
        }
    }

    fn assign(&mut self, scope: &BlockScope, id: VarId, expr: Expr, op: AssignOperation) {
        self.ins(Ins::PushInt(id.0 as IntValue));

        self.expr(scope, expr);

        self.ins(match op {
            AssignOperation::None => Ins::Assign,
            AssignOperation::Add => Ins::AssignAdd,
            AssignOperation::Sub => Ins::AssignSub,
            AssignOperation::Mul => Ins::AssignMul,
            AssignOperation::Div => Ins::AssignDiv,
            AssignOperation::Mod => Ins::AssignMod,
        });

        self.ins(Ins::Discard);
    }

    fn stmt(&mut self, scope: &mut BlockScope, stmt: Stmt) {
        match stmt {
            Stmt::Vars(vars) => {
                for (name, expr) in vars {
                    if let Some(id) = scope.define_var(name.clone()) {
                        if let Some(expr) = expr {
                            self.assign(scope, id, expr, AssignOperation::None);
                        }
                    } else {
                        self.err.push(NameAlreadyDeclared(name));
                    }
                }
            }

            Stmt::Consts(inits) => {
                for (name, expr) in inits {
                    if let Some(val) = ConstVal::eval_expr(&expr, scope) {
                        if let None = scope.define_const(name.clone(), val) {
                            self.err.push(NameAlreadyDeclared(name));
                        }
                    } else {
                        self.err.push(FailedConstantEvaluation)
                    }
                }
            }

            Stmt::Assign(op, name, expr) => {
                if let Some(name_ref) = scope.lookup_name(&name) {
                    if let NameRef::Var(id) = name_ref {
                        self.assign(scope, id, expr, op);
                    } else {
                        self.err.push(CannotAssignToNonVar(name));
                    }
                } else {
                    self.err.push(NameNotDeclared(name));
                }
            }

            Stmt::Expr(expr) => {
                self.expr(scope, expr);
                self.ins(Ins::Discard);
            }

            Stmt::Call(invoke) => match scope.lookup_name(&invoke.func) {
                Some(NameRef::Func(cid)) => {
                    for arg in invoke.args {
                        self.expr(scope, arg);
                    }

                    self.ins(Ins::Call(cid));
                    self.ins(Ins::Discard);
                }

                Some(NameRef::Proc(cid)) => {
                    // same as func, but no discard

                    for arg in invoke.args {
                        self.expr(scope, arg);
                    }

                    self.ins(Ins::Call(cid));
                }

                Some(_) => self.err.push(NotCallable(invoke.func)),
                _ => self.err.push(NameNotDeclared(invoke.func)),
            },

            Stmt::If(expr, stmts) => {
                let next_lab = self.new_label();

                self.expr(scope, expr);
                self.ins(Ins::Beq(next_lab));
                self.stmts(scope, stmts);
                self.ins(Ins::Label(next_lab));
            }

            Stmt::IfElse(expr, true_stmts, false_stmts) => {
                let else_lab = self.new_label();
                let next_lab = self.new_label();

                self.expr(scope, expr);
                self.ins(Ins::Beq(else_lab));
                self.stmts(scope, true_stmts);
                self.ins(Ins::Jmp(next_lab));
                self.ins(Ins::Label(else_lab));
                self.stmts(scope, false_stmts);
                self.ins(Ins::Label(next_lab));
            }

            Stmt::For(elements) => {
                let (expr, head, tail, body) = *elements;

                // for have special scopes for the statements within the for construct
                // this is where 'i' goes when writing 'for var i = 0; i < MAX; i++ { ... }'

                let mut for_scope = BlockScope::new(scope.next_id(), scope);

                let loop_lab = self.new_label();
                let tail_lab = self.new_label();
                let body_lab = self.new_label();
                let next_lab = self.new_label();

                self.stmt(&mut for_scope, head);

                self.ins(Ins::Label(loop_lab));
                self.expr(&for_scope, expr);
                self.ins(Ins::Bne(next_lab));
                self.ins(Ins::Jmp(body_lab));

                self.ins(Ins::Label(tail_lab));
                self.stmt(&mut for_scope, tail);
                self.ins(Ins::Jmp(loop_lab));

                self.ins(Ins::Label(body_lab));
                self.stmts(&for_scope, body);
                self.ins(Ins::Jmp(tail_lab));

                self.ins(Ins::Label(next_lab));
            }

            Stmt::DoWhile(expr, body) => {
                let loop_lab = self.new_label();

                self.ins(Ins::Label(loop_lab));
                self.stmts(scope, body);
                self.expr(scope, expr);
                self.ins(Ins::Bne(loop_lab))
            }

            Stmt::Switch(expr, cases) => {
                let switch_lab = self.new_label();
                let next_lab = self.new_label();
                let switch_id = self.new_switch();

                self.expr(scope, expr);
                self.ins(Ins::Jmp(switch_lab));

                let mut found_default = false;

                for case in cases {
                    match case {
                        SwitchCase::Case(expr, stmts) => match ConstVal::eval_expr(&expr, scope) {
                            Some(ConstVal::Int(val)) => {
                                self.ins(Ins::Case(switch_id, CaseEnum::Val(val)));
                                self.stmts(scope, stmts);
                                self.ins(Ins::Jmp(next_lab));
                            }

                            Some(ConstVal::Str(_)) => self.err.push(ExpectedConstantIntGotStr),
                            None => self.err.push(FailedConstantEvaluation),
                        },

                        SwitchCase::Default(stmts) => {
                            if found_default {
                                self.err.push(MultipleDefaults);
                            }

                            found_default = true;
                            self.ins(Ins::Case(switch_id, CaseEnum::Default));
                            self.stmts(scope, stmts);
                            self.ins(Ins::Jmp(next_lab));
                        }
                    }
                }

                self.ins(Ins::Jmp(next_lab)); // dead, but needed to produce matching code
                self.ins(Ins::Label(switch_lab));
                self.ins(Ins::Switch(switch_id));
                self.ins(Ins::Label(next_lab));
            }
        }
    }

    fn stmts(&mut self, parent_scope: &dyn NameAccess, stmts: Vec<Stmt>) {
        let mut scope = BlockScope::new(0, parent_scope);

        for stmt in stmts {
            self.stmt(&mut scope, stmt);
        }
    }

    fn end(self) -> Result<Script, CompileErrors> {
        if self.err.len() != 0 {
            Err(CompileErrors(self.err))
        } else {
            Ok(Script::new(self.vec, self.str))
        }
    }
}

pub fn compile_script(stmts: Vec<Stmt>, const_scope: &ConstScope) -> Result<Script, CompileErrors> {
    let mut emit = Emit::new();
    emit.stmts(const_scope, stmts);
    emit.end()
}
