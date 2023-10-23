use pomelo::pomelo;
use thiserror::Error;

use crate::ast::{AssignOperation, ConstVal, Expr, Invoke, Stmt, SwitchCase};
use crate::compiler::compile_script;
use crate::compiler::{CompileErrors, ConstScope};
use crate::ir::{CallId, IntValue, Script};

pub struct ParseContext {
    pub allow_scripts: bool,
    pub allow_declarations: bool,
    pub scripts: Vec<(IntValue, String, Script)>,
    pub const_scope: ConstScope,
}

impl ParseContext {
    pub fn new() -> Self {
        Self {
            allow_scripts: true,
            allow_declarations: true,
            scripts: Vec::new(),
            const_scope: ConstScope::new(),
        }
    }
}

#[derive(Debug, Error)]
pub enum ScriptError {
    #[error("Compile errors while compiling {0}: {1}")]
    CompileErrors(String, CompileErrors),

    #[error("Syntax error")]
    SyntaxError,

    #[error("Fatal syntax error")]
    ParseFail,

    #[error("Fatal parse error: stack overflow")]
    ParseStackOverflow,

    #[error("Failed to evaluate constant expression")]
    FailedConstantEvaluation,

    #[error("Toplevel declarations must be before script blocks")]
    DeclarationsAfterScript,

    #[error("Included files cannot define script blocks")]
    ScriptInIncludedFiles,

    #[error("{0} ids cannot be strings")]
    StringId(&'static str),
}

pomelo! {
    %include { use super::*; }

    %extra_argument ParseContext;

    // token types

    %type Integer IntValue;
    %type Name String;
    %type StringLit Vec<u8>;

    // errors

    %error ScriptError;

    %syntax_error { Err(ScriptError::SyntaxError) }

    %parse_fail { ScriptError::ParseFail }
    %stack_overflow { ScriptError::ParseStackOverflow }

    // grammar

    program ::= declarations;

    declarations ::= ;
    declarations ::= declarations declaration;

    declaration ::= const_declaration;
    declaration ::= function_declaration;
    declaration ::= script_declaration;

    const_declaration ::= KwConst Name(name) Equal expr(n) {
        if extra.allow_declarations {
            match ConstVal::eval_expr(&n, &extra.const_scope) {
                Some(val) => extra.const_scope.add_const(name, val),
                None => return Err(ScriptError::FailedConstantEvaluation),
            }
        } else {
            return Err(ScriptError::DeclarationsAfterScript);
        }
    };

    function_declaration ::= KwFunc primary_expr(e) Name(n) paren_params {
        if extra.allow_declarations {
            match ConstVal::eval_expr(&e, &extra.const_scope) {
                Some(ConstVal::Int(integer)) => extra.const_scope.add_func(n, CallId(integer as usize)),
                Some(ConstVal::Str(_)) => return Err(ScriptError::StringId("Function")),
                None => return Err(ScriptError::FailedConstantEvaluation),
            }
        } else {
            return Err(ScriptError::DeclarationsAfterScript);
        }
    };

    function_declaration ::= KwProc primary_expr(e) Name(n) paren_params {
        if extra.allow_declarations {
            match ConstVal::eval_expr(&e, &extra.const_scope) {
                Some(ConstVal::Int(integer)) => extra.const_scope.add_proc(n, CallId(integer as usize)),
                Some(ConstVal::Str(_)) => return Err(ScriptError::StringId("Procedure")),
                None => return Err(ScriptError::FailedConstantEvaluation),
            }
        } else {
            return Err(ScriptError::DeclarationsAfterScript);
        }
    };

    paren_params ::= LParen RParen;
    paren_params ::= LParen params RParen;

    params ::= param;
    params ::= params Comma param;

    param ::= Name;

    script_declaration ::= KwScript expr(e) Name(n) stmt_block(b) {
        if extra.allow_scripts {
            // don't allow declarations after the first script
            // we could enforce this through the syntax but that would lead in less helpful error messages
            extra.allow_declarations = false;

            let id = match ConstVal::eval_expr(&e, &extra.const_scope) {
                Some(ConstVal::Int(id)) => id,
                Some(ConstVal::Str(_)) => return Err(ScriptError::StringId("Script")),
                None => return Err(ScriptError::FailedConstantEvaluation),
            };

            let compiled = compile_script(b, &extra.const_scope);

            match compiled {
                Ok(compiled) => extra.scripts.push((id, n, compiled)),
                Err(ce) => return Err(ScriptError::CompileErrors(n, ce)),
            }
        } else {
            return Err(ScriptError::ScriptInIncludedFiles);
        }
    };

    /* ============== */
    /* = Statements = */
    /* ============== */

    %type stmt_block Vec<Stmt>;
    stmt_block ::= LCurly stmts RCurly;
    stmt_block ::= LCurly error RCurly { Vec::new() };

    %type stmts Vec<Stmt>;
    stmts ::= stmts(mut v) stmt(s) { v.push(s); v };
    stmts ::= { Vec::new() };

    %type stmt Stmt;
    stmt ::= KwVar initializers(i) { Stmt::Vars(i) };
    stmt ::= KwConst const_initializers(i) { Stmt::Consts(i) };
    stmt ::= assignment(a) { Stmt::Assign(a.0, a.1, a.2) };
    stmt ::= call(i) { Stmt::Call(i) }
    stmt ::= KwIf expr(e) stmt_block(b) { Stmt::If(e, b) };
    stmt ::= KwIf expr(e) stmt_block(b1) KwElse stmt_block(b2) { Stmt::IfElse(e, b1, b2) };
    stmt ::= KwDo stmt_block(b) KwWhile expr(e) { Stmt::DoWhile(e,b) };
    stmt ::= KwFor stmt(h) Semicolon expr(e) Semicolon stmt(t) stmt_block(b) { Stmt::For(Box::new((e, h, t, b))) };
    stmt ::= KwSwitch expr(e) LCurly cases(c) RCurly { Stmt::Switch(e, c) };
    stmt ::= preincr(e) { Stmt::Expr(e) };
    stmt ::= postincr(e) { Stmt::Expr(e) };

    %type initializers Vec<(String, Option<Expr>)>;
    initializers ::= initializers(mut v) Comma initializer(i) { v.push(i); v };
    initializers ::= initializer(i) { vec![i] };

    %type initializer (String, Option<Expr>);
    initializer ::= Name(n) { (n, None) };
    initializer ::= Name(n) Equal expr(e) { (n, Some(e)) };

    %type const_initializers Vec<(String, Expr)>;
    const_initializers ::= const_initializers(mut v) Comma const_initializer(i) { v.push(i); v };
    const_initializers ::= const_initializer(i) { vec![i] };

    %type const_initializer (String, Expr);
    const_initializer ::= Name(n) Equal expr(e) { (n, e) };

    %type cases Vec<SwitchCase>;
    cases ::= cases(mut v) case(c) { v.push(c); v };
    cases ::= case(c) { vec![c] };

    %type case SwitchCase;
    case ::= KwCase expr(e) stmt_block(b) { SwitchCase::Case(e, b) };
    case ::= KwDefault stmt_block(b) { SwitchCase::Default(b) };

    /* =============== */
    /* = Expressions = */
    /* =============== */

    %type assignment (AssignOperation, String, Expr);
    assignment ::= Name(n) Equal expr(e) { (AssignOperation::None, n, e) };
    assignment ::= Name(n) AddEqual expr(e) { (AssignOperation::Add, n, e) };
    assignment ::= Name(n) SubEqual expr(e) { (AssignOperation::Sub, n, e) };
    assignment ::= Name(n) MulEqual expr(e) { (AssignOperation::Mul, n, e) };
    assignment ::= Name(n) DivEqual expr(e) { (AssignOperation::Div, n, e) };
    assignment ::= Name(n) ModEqual expr(e) { (AssignOperation::Mod, n, e) };

    %type call Invoke;
    call ::= Name(n) LParen RParen { Invoke::new(n, Vec::new()) };
    call ::= Name(n) LParen args(a) RParen { Invoke::new(n, a) };

    %type args Vec<Expr>;
    args ::= expr(e) { vec![e] };
    args ::= args(mut a) Comma expr(e) { a.push(e); a };

    %type expr Expr;
    // expr ::= assignment;
    expr ::= conditional_expr;

    %type conditional_expr Expr;
    conditional_expr ::= or_expr;

    %type or_expr Expr;
    or_expr ::= or_expr(a) LOr and_expr(b) { Expr::OpOr(Box::new((a, b))) };
    or_expr ::= and_expr;

    %type and_expr Expr;
    and_expr ::= and_expr(a) LAnd equality_expr(b) { Expr::OpAnd(Box::new((a, b))) };
    and_expr ::= equality_expr;

    %type equality_expr Expr;
    equality_expr ::= equality_expr(a) CompareEq relation_expr(b) { Expr::CmpEq(Box::new((a, b))) };
    equality_expr ::= equality_expr(a) CompareNe relation_expr(b) { Expr::CmpNe(Box::new((a, b))) };
    equality_expr ::= relation_expr;

    %type relation_expr Expr;
    relation_expr ::= relation_expr(a) CompareLt add_expr(b) { Expr::CmpLt(Box::new((a, b))) };
    relation_expr ::= relation_expr(a) CompareLe add_expr(b) { Expr::CmpLe(Box::new((a, b))) };
    relation_expr ::= relation_expr(a) CompareGe add_expr(b) { Expr::CmpGe(Box::new((a, b))) };
    relation_expr ::= relation_expr(a) CompareGt add_expr(b) { Expr::CmpGt(Box::new((a, b))) };
    relation_expr ::= add_expr;

    %type add_expr Expr;
    add_expr ::= add_expr(a) Plus mul_expr(b) { Expr::OpAdd(Box::new((a, b))) };
    add_expr ::= add_expr(a) Minus mul_expr(b) { Expr::OpSub(Box::new((a, b))) };
    add_expr ::= mul_expr;

    %type mul_expr Expr;
    mul_expr ::= mul_expr(a) Times unary_expr(b) { Expr::OpMul(Box::new((a, b))) };
    mul_expr ::= mul_expr(a) Divide unary_expr(b) { Expr::OpDiv(Box::new((a, b))) };
    mul_expr ::= mul_expr(a) Modulus unary_expr(b) { Expr::OpMod(Box::new((a, b))) };
    mul_expr ::= unary_expr;

    %type unary_expr Expr;
    unary_expr ::= Minus unary_expr(e) { Expr::OpNeg(Box::new(e)) };
    unary_expr ::= Plus unary_expr;
    unary_expr ::= Negate unary_expr(e) { Expr::OpNot(Box::new(e)) };
    unary_expr ::= postfix_expr;

    %type postfix_expr Expr;
    postfix_expr ::= call(i) { Expr::Call(i) };
    postfix_expr ::= primary_expr;

    %type primary_expr Expr;
    primary_expr ::= Name(n) { Expr::Name(n) };
    primary_expr ::= Integer(i) { Expr::Int(i) };
    primary_expr ::= StringLit(s) { Expr::Str(s) };
    primary_expr ::= LParen expr RParen;
    primary_expr ::= LParen postincr RParen;
    primary_expr ::= LParen preincr RParen;

    %type preincr Expr;
    preincr ::= PlusPlus Name(n) { Expr::PreIncrement(n) };
    preincr ::= MinusMinus Name(n) { Expr::PreDecrement(n) };

    %type postincr Expr;
    postincr ::= Name(n) PlusPlus { Expr::PostIncrement(n) };
    postincr ::= Name(n) MinusMinus { Expr::PostDecrement(n) };
}

pub use parser::*;
