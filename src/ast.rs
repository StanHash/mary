use crate::ir::{IntValue, StrValue};

#[derive(Debug)]
pub struct Invoke {
    pub func: String,
    pub args: Vec<Expr>,
}

impl Invoke {
    pub fn new(func: String, args: Vec<Expr>) -> Self {
        Self { func, args }
    }
}

#[derive(Debug)]
pub enum Expr {
    Name(String),
    Int(IntValue),
    Str(Vec<u8>),
    OpAdd(Box<Expr>, Box<Expr>),
    OpSub(Box<Expr>, Box<Expr>),
    OpMul(Box<Expr>, Box<Expr>),
    OpDiv(Box<Expr>, Box<Expr>),
    OpMod(Box<Expr>, Box<Expr>),
    OpOr(Box<Expr>, Box<Expr>),
    OpAnd(Box<Expr>, Box<Expr>),
    OpNeg(Box<Expr>),
    OpNot(Box<Expr>),
    PostIncrement(String),
    PreIncrement(String),
    PostDecrement(String),
    PreDecrement(String),
    CmpEq(Box<Expr>, Box<Expr>),
    CmpNe(Box<Expr>, Box<Expr>),
    CmpLt(Box<Expr>, Box<Expr>),
    CmpLe(Box<Expr>, Box<Expr>),
    CmpGe(Box<Expr>, Box<Expr>),
    CmpGt(Box<Expr>, Box<Expr>),
    Call(Invoke),
}

impl Expr {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug)]
pub enum AssignOperation {
    None,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum Stmt {
    Vars(Vec<(String, Option<Expr>)>),
    Consts(Vec<(String, Expr)>),
    Assign(AssignOperation, String, Expr),
    Call(Invoke),
    If(Expr, Vec<Stmt>),
    IfElse(Expr, Vec<Stmt>, Vec<Stmt>),
    For(Box<Stmt>, Expr, Box<Stmt>, Vec<Stmt>),
    DoWhile(Vec<Stmt>, Expr),
    Switch(Expr, Vec<SwitchCase>),
}

impl Stmt {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug)]
pub enum SwitchCase {
    Case(Expr, Vec<Stmt>),
    Default(Vec<Stmt>),
}

// TODO: do we need ConstVal? can't we have ConstRef?

pub enum ConstVal {
    Int(IntValue),
    Str(StrValue),
}

pub enum ConstRef<'a> {
    Int(IntValue),
    Str(&'a StrValue),
}

pub trait ConstAccess {
    fn lookup_const(&self, name: &str) -> Option<ConstRef>;
}

impl ConstVal {
    fn binop<F>(lhs: Option<ConstVal>, rhs: Option<ConstVal>, f: F) -> Option<ConstVal>
    where
        F: Fn(IntValue, IntValue) -> IntValue,
    {
        if let Some(Self::Int(lv)) = lhs {
            if let Some(Self::Int(rv)) = rhs {
                return Some(Self::Int(f(lv, rv)));
            }
        }

        None
    }

    fn add(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| a + b)
    }

    fn sub(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| a - b)
    }

    fn mul(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| a * b)
    }

    fn div(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| a / b)
    }

    fn rem(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| a % b)
    }

    fn or(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| if a != 0 || b != 0 { 1 } else { 0 })
    }

    fn and(lhs: Option<ConstVal>, rhs: Option<ConstVal>) -> Option<ConstVal> {
        Self::binop(lhs, rhs, |a, b| if a != 0 && b != 0 { 1 } else { 0 })
    }

    fn neg(inner: Option<ConstVal>) -> Option<ConstVal> {
        if let Some(Self::Int(i)) = inner {
            Some(Self::Int(-i))
        } else {
            None
        }
    }

    fn not(inner: Option<ConstVal>) -> Option<ConstVal> {
        if let Some(Self::Int(i)) = inner {
            Some(Self::Int(if i != 0 { 0 } else { 1 }))
        } else {
            None
        }
    }

    pub fn eval_expr<A: ConstAccess>(expr: &Expr, a: &A) -> Option<ConstVal> {
        let eval = |expr| Self::eval_expr(expr, a);

        match expr {
            Expr::Name(name) => match a.lookup_const(name) {
                Some(ConstRef::Int(i)) => Some(Self::Int(i)),
                Some(ConstRef::Str(s)) => Some(Self::Str(s.clone())),
                None => todo!(), // TODO: error
            },

            Expr::Int(i) => Some(Self::Int(*i)),
            Expr::Str(s) => Some(Self::Str(s.clone())),
            Expr::OpAdd(lhs, rhs) => Self::add(eval(lhs), eval(rhs)),
            Expr::OpSub(lhs, rhs) => Self::sub(eval(lhs), eval(rhs)),
            Expr::OpMul(lhs, rhs) => Self::mul(eval(lhs), eval(rhs)),
            Expr::OpDiv(lhs, rhs) => Self::div(eval(lhs), eval(rhs)),
            Expr::OpMod(lhs, rhs) => Self::rem(eval(lhs), eval(rhs)),
            Expr::OpOr(lhs, rhs) => Self::or(eval(lhs), eval(rhs)),
            Expr::OpAnd(lhs, rhs) => Self::and(eval(lhs), eval(rhs)),
            Expr::OpNeg(inner) => Self::neg(eval(inner)),
            Expr::OpNot(inner) => Self::not(eval(inner)),

            Expr::PostIncrement(_) => todo!(), // TODO: error
            Expr::PreIncrement(_) => todo!(),  // TODO: error
            Expr::PostDecrement(_) => todo!(), // TODO: error
            Expr::PreDecrement(_) => todo!(),  // TODO: error
            Expr::CmpEq(_, _) => todo!(),      // TODO
            Expr::CmpNe(_, _) => todo!(),      // TODO
            Expr::CmpLt(_, _) => todo!(),      // TODO
            Expr::CmpLe(_, _) => todo!(),      // TODO
            Expr::CmpGe(_, _) => todo!(),      // TODO
            Expr::CmpGt(_, _) => todo!(),      // TODO
            Expr::Call(_) => todo!(),          // TODO: error
        }
    }
}
