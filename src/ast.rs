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
    OpAdd(Box<(Expr, Expr)>),
    OpSub(Box<(Expr, Expr)>),
    OpMul(Box<(Expr, Expr)>),
    OpDiv(Box<(Expr, Expr)>),
    OpMod(Box<(Expr, Expr)>),
    OpOr(Box<(Expr, Expr)>),
    OpAnd(Box<(Expr, Expr)>),
    OpNeg(Box<Expr>),
    OpNot(Box<Expr>),
    PostIncrement(String),
    PreIncrement(String),
    PostDecrement(String),
    PreDecrement(String),
    CmpEq(Box<(Expr, Expr)>),
    CmpNe(Box<(Expr, Expr)>),
    CmpLt(Box<(Expr, Expr)>),
    CmpLe(Box<(Expr, Expr)>),
    CmpGe(Box<(Expr, Expr)>),
    CmpGt(Box<(Expr, Expr)>),
    Call(Invoke),
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
    Expr(Expr),
    Call(Invoke),
    If(Expr, Vec<Stmt>),
    IfElse(Expr, Vec<Stmt>, Vec<Stmt>),
    For(Box<(Expr, Stmt, Stmt, Vec<Stmt>)>),
    DoWhile(Expr, Vec<Stmt>),
    Switch(Expr, Vec<SwitchCase>),
}

#[derive(Debug)]
pub enum SwitchCase {
    Case(Expr, Vec<Stmt>),
    Default(Vec<Stmt>),
}

// TODO: do we need ConstVal? can't we have ConstRef?

#[derive(Debug, PartialEq, Eq)]
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
            Expr::OpAdd(ops) => Self::add(eval(&ops.0), eval(&ops.1)),
            Expr::OpSub(ops) => Self::sub(eval(&ops.0), eval(&ops.1)),
            Expr::OpMul(ops) => Self::mul(eval(&ops.0), eval(&ops.1)),
            Expr::OpDiv(ops) => Self::div(eval(&ops.0), eval(&ops.1)),
            Expr::OpMod(ops) => Self::rem(eval(&ops.0), eval(&ops.1)),
            Expr::OpOr(ops) => Self::or(eval(&ops.0), eval(&ops.1)),
            Expr::OpAnd(ops) => Self::and(eval(&ops.0), eval(&ops.1)),
            Expr::OpNeg(inner) => Self::neg(eval(inner)),
            Expr::OpNot(inner) => Self::not(eval(inner)),

            Expr::PostIncrement(_) => todo!(), // TODO: error
            Expr::PreIncrement(_) => todo!(),  // TODO: error
            Expr::PostDecrement(_) => todo!(), // TODO: error
            Expr::PreDecrement(_) => todo!(),  // TODO: error
            Expr::CmpEq(_) => todo!(),         // TODO
            Expr::CmpNe(_) => todo!(),         // TODO
            Expr::CmpLt(_) => todo!(),         // TODO
            Expr::CmpLe(_) => todo!(),         // TODO
            Expr::CmpGe(_) => todo!(),         // TODO
            Expr::CmpGt(_) => todo!(),         // TODO
            Expr::Call(_) => todo!(),          // TODO: error
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_const_eval() {
        let int_pairs = [(5, 3), (0, 1), (6, 7)];

        for (a, b) in int_pairs {
            assert_eq!(
                ConstVal::add(Some(ConstVal::Int(a)), Some(ConstVal::Int(b))),
                Some(ConstVal::Int(a + b))
            );

            assert_eq!(
                ConstVal::sub(Some(ConstVal::Int(a)), Some(ConstVal::Int(b))),
                Some(ConstVal::Int(a - b))
            );

            assert_eq!(
                ConstVal::mul(Some(ConstVal::Int(a)), Some(ConstVal::Int(b))),
                Some(ConstVal::Int(a * b))
            );

            assert_eq!(
                ConstVal::div(Some(ConstVal::Int(a)), Some(ConstVal::Int(b))),
                Some(ConstVal::Int(a / b))
            );

            assert_eq!(
                ConstVal::rem(Some(ConstVal::Int(a)), Some(ConstVal::Int(b))),
                Some(ConstVal::Int(a % b))
            );
        }
    }
}
