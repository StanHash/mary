use std::{fmt, slice};

use crate::ast::{Expr, Invoke, Stmt, SwitchCase};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    // Primary,
    // Call,
    Unary,
    Multiplicative,
    Additive,
    Relational,
    Equality,
    And,
    Or,
    Top,
}

#[derive(Debug)]
pub struct PrettyExpr<'a>(&'a Expr, Precedence);

impl<'a> PrettyExpr<'a> {
    pub fn new(expr: &'a Expr) -> Self {
        Self(expr, Precedence::Top)
    }

    fn with_precedence(expr: &'a Expr, precedence: Precedence) -> Self {
        Self(expr, precedence)
    }
}

impl<'a> fmt::Display for PrettyExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Expr::Name(name) => write!(f, "{0}", name),
            Expr::Int(int_value) => write!(f, "{0}", int_value),
            Expr::Str(items) => PrettyStringLit::new(items).fmt(f),

            Expr::OpAdd(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Additive);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Multiplicative);
                self.fmt_binop(f, pl, "+", pr, Precedence::Additive)
            }

            Expr::OpSub(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Additive);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Multiplicative);
                self.fmt_binop(f, pl, "-", pr, Precedence::Additive)
            }

            Expr::OpMul(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Multiplicative);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Unary);
                self.fmt_binop(f, pl, "*", pr, Precedence::Multiplicative)
            }

            Expr::OpDiv(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Multiplicative);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Unary);
                self.fmt_binop(f, pl, "/", pr, Precedence::Multiplicative)
            }

            Expr::OpMod(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Multiplicative);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Unary);
                self.fmt_binop(f, pl, "%", pr, Precedence::Multiplicative)
            }

            Expr::OpOr(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Or);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::And);
                self.fmt_binop(f, pl, "||", pr, Precedence::Or)
            }

            Expr::OpAnd(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::And);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Equality);
                self.fmt_binop(f, pl, "&&", pr, Precedence::And)
            }

            Expr::OpNeg(expr) => {
                let pi = PrettyExpr::with_precedence(expr, Precedence::Unary);
                write!(f, "-{0}", pi)
            }

            Expr::OpNot(expr) => {
                let pi = PrettyExpr::with_precedence(expr, Precedence::Unary);
                write!(f, "!{0}", pi)
            }

            Expr::PostIncrement(name) => self.fmt_prepost(f, "", name, "++"),
            Expr::PreIncrement(name) => self.fmt_prepost(f, "++", name, ""),
            Expr::PostDecrement(name) => self.fmt_prepost(f, "", name, "--"),
            Expr::PreDecrement(name) => self.fmt_prepost(f, "--", name, ""),

            Expr::CmpEq(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Equality);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Relational);
                self.fmt_binop(f, pl, "==", pr, Precedence::Equality)
            }

            Expr::CmpNe(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Equality);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Relational);
                self.fmt_binop(f, pl, "!=", pr, Precedence::Equality)
            }

            Expr::CmpLt(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Relational);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Additive);
                self.fmt_binop(f, pl, "<", pr, Precedence::Relational)
            }

            Expr::CmpLe(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Relational);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Additive);
                self.fmt_binop(f, pl, "<=", pr, Precedence::Relational)
            }

            Expr::CmpGe(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Relational);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Additive);
                self.fmt_binop(f, pl, ">=", pr, Precedence::Relational)
            }

            Expr::CmpGt(exprs) => {
                let pl = PrettyExpr::with_precedence(&exprs.0, Precedence::Relational);
                let pr = PrettyExpr::with_precedence(&exprs.1, Precedence::Additive);
                self.fmt_binop(f, pl, ">", pr, Precedence::Relational)
            }

            Expr::Call(invoke) => fmt_invoke(f, invoke),
        }
    }
}

impl<'a> PrettyExpr<'a> {
    fn fmt_binop(
        &self,
        f: &mut fmt::Formatter<'_>,
        pl: PrettyExpr,
        op: &str,
        pr: PrettyExpr,
        precedence: Precedence,
    ) -> fmt::Result {
        if self.1 < precedence {
            write!(f, "({0} {2} {1})", pl, pr, op)
        } else {
            write!(f, "{0} {2} {1}", pl, pr, op)
        }
    }

    fn fmt_prepost(
        &self,
        f: &mut fmt::Formatter<'_>,
        pre: &str,
        name: &str,
        post: &str,
    ) -> fmt::Result {
        if self.1 < Precedence::Top {
            write!(f, "({1}{0}{2})", name, pre, post)
        } else {
            write!(f, "{1}{0}{2}", name, pre, post)
        }
    }
}

#[derive(Debug)]
pub struct PrettyStmts<'a>(&'a [Stmt], usize);

impl<'a> PrettyStmts<'a> {
    pub fn new(stmts: &'a [Stmt]) -> Self {
        Self(stmts, 0)
    }

    pub fn with_indent(stmts: &'a [Stmt], indent: usize) -> Self {
        Self(stmts, indent)
    }
}

impl<'a> fmt::Display for PrettyStmts<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent_string = "    ".repeat(self.1);

        let mut first = true;

        for stmt in self.0 {
            if !first {
                /* statement separator */
                writeln!(f)?;
            }

            match stmt {
                Stmt::Vars(items) => {
                    /* var {name}[ = {expr}][, ...] */
                    write!(f, "{indent_string}var ")?;

                    let (name, expr) = &items[0];

                    write!(f, "{name}")?;

                    if let Some(expr) = expr {
                        let pretty_expr = PrettyExpr::new(expr);
                        write!(f, " = {pretty_expr}")?;
                    }

                    for (name, expr) in &items[1..] {
                        write!(f, ", {name}")?;

                        if let Some(expr) = expr {
                            let pretty_expr = PrettyExpr::new(expr);
                            write!(f, " = {pretty_expr}")?;
                        }
                    }
                }

                Stmt::Consts(items) => {
                    /* const {name} = {expr}[, ...] */
                    write!(f, "{indent_string}const ")?;

                    let (name, expr) = &items[0];
                    let pretty_expr = PrettyExpr::new(expr);
                    write!(f, "{name} = {pretty_expr}")?;

                    for (name, expr) in &items[1..] {
                        let pretty_expr = PrettyExpr::new(expr);
                        write!(f, ", {name} = {pretty_expr}")?;
                    }
                }

                Stmt::Assign(assign_operation, var_name, expr) => {
                    write!(f, "{indent_string}{var_name} {assign_operation}")?;

                    let pretty_expr = PrettyExpr::new(expr);
                    write!(f, " {pretty_expr}")?;
                }

                Stmt::Expr(expr) => {
                    let pretty_expr = PrettyExpr::new(expr);
                    write!(f, "{indent_string}{pretty_expr}")?;
                }

                Stmt::Call(invoke) => {
                    write!(f, "{indent_string}")?;
                    fmt_invoke(f, invoke)?;
                }

                Stmt::If(expr, stmts) => {
                    let pretty_expr = PrettyExpr::new(expr);

                    writeln!(f, "{indent_string}if {pretty_expr}")?;
                    writeln!(f, "{indent_string}{{")?;

                    let pretty_body = PrettyStmts::with_indent(&stmts[..], self.1 + 1);
                    writeln!(f, "{pretty_body}")?;

                    write!(f, "{indent_string}}}")?;
                }

                Stmt::IfElse(expr, stmts0, stmts1) => {
                    let pretty_expr = PrettyExpr::new(expr);

                    writeln!(f, "{indent_string}if {pretty_expr}")?;
                    writeln!(f, "{indent_string}{{")?;

                    let pretty_body = PrettyStmts::with_indent(&stmts0[..], self.1 + 1);
                    writeln!(f, "{pretty_body}")?;

                    writeln!(f, "{indent_string}}}")?;

                    match &stmts1[..] {
                        [Stmt::IfElse(_, _, _)] | [Stmt::If(_, _)] => {
                            /* special case, we flatten this else if chain
                             * TODO: support in compiler */

                            let pretty_body = PrettyStmts::with_indent(&stmts1[..], self.1);

                            /* HACK: we don't want the include the indent of the body, so we eat it manually */
                            let body_string = format!("{pretty_body}");
                            let body_string = &body_string[indent_string.len()..];

                            write!(f, "{indent_string}else {body_string}")?;
                        }

                        _ => {
                            writeln!(f, "{indent_string}else")?;
                            writeln!(f, "{indent_string}{{")?;

                            let pretty_body = PrettyStmts::with_indent(&stmts1[..], self.1 + 1);
                            writeln!(f, "{pretty_body}")?;

                            write!(f, "{indent_string}}}")?;
                        }
                    }
                }

                Stmt::For(for_box) => {
                    let (expr, stmt_init, stmt_iter, body) = &**for_box;

                    let pretty_init = PrettyStmts::new(slice::from_ref(stmt_init));
                    let pretty_expr = PrettyExpr::new(expr);
                    let pretty_iter = PrettyStmts::new(slice::from_ref(stmt_iter));

                    writeln!(
                        f,
                        "{indent_string}for {pretty_init}; {pretty_expr}; {pretty_iter}"
                    )?;

                    writeln!(f, "{indent_string}{{")?;

                    let pretty_body = PrettyStmts::with_indent(&body[..], self.1 + 1);
                    writeln!(f, "{pretty_body}")?;

                    write!(f, "{indent_string}}}")?;
                }

                Stmt::DoWhile(expr, stmts) => {
                    writeln!(f, "{indent_string}do")?;
                    writeln!(f, "{indent_string}{{")?;

                    let pretty_body = PrettyStmts::with_indent(&stmts[..], self.1 + 1);
                    writeln!(f, "{pretty_body}")?;

                    let pretty_expr = PrettyExpr::new(expr);
                    write!(f, "{indent_string}}} while {pretty_expr}")?;
                }

                Stmt::Switch(expr, switch_cases, _) => {
                    let pretty_expr = PrettyExpr::new(expr);

                    writeln!(f, "{indent_string}switch {pretty_expr}")?;
                    writeln!(f, "{indent_string}{{")?;

                    for switch_case in switch_cases {
                        fmt_switch_case(f, switch_case, self.1 + 1)?;
                        writeln!(f)?;
                    }

                    write!(f, "{indent_string}}}")?;
                }

                Stmt::Exit => write!(f, "{indent_string}exit")?,
            }

            first = false;
        }

        Ok(())
    }
}

pub struct PrettyStringLit<'a>(&'a [u8]);

impl<'a> PrettyStringLit<'a> {
    pub fn new(string_lit: &'a [u8]) -> Self {
        Self(string_lit)
    }
}

impl<'a> fmt::Display for PrettyStringLit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;

        for byte in self.0 {
            match byte {
                b'"' => write!(f, "\\\"")?,
                b'\n' => write!(f, "\\n")?,
                b'\r' => write!(f, "\\r")?,
                b'\t' => write!(f, "\\t")?,
                b'\\' => write!(f, "\\\\")?,
                0x20..0x7F => write!(f, "{0}", *byte as char)?,
                _ => write!(f, "\\x{0:02X}", byte)?,
            }
        }

        write!(f, "\"")
    }
}

fn fmt_args(f: &mut fmt::Formatter<'_>, args: &[Expr]) -> fmt::Result {
    let mut first = true;

    for arg in args {
        let pretty_expr = PrettyExpr::new(arg);

        if first {
            write!(f, "{pretty_expr}")
        } else {
            write!(f, ", {pretty_expr}")
        }?;

        first = false;
    }

    Ok(())
}

// TODO: PrettyInvoke
pub(crate) fn fmt_invoke(f: &mut fmt::Formatter<'_>, invoke: &Invoke) -> fmt::Result {
    write!(f, "{0}(", invoke.func)?;
    fmt_args(f, &invoke.args)?;
    write!(f, ")")
}

// TODO: PrettySwitchCase
pub(crate) fn fmt_switch_case(
    f: &mut fmt::Formatter<'_>,
    switch_case: &SwitchCase,
    indent: usize,
) -> Result<(), fmt::Error> {
    let indent_string = "    ".repeat(indent);

    Ok(match switch_case {
        SwitchCase::Case(exprs, stmts) => {
            write!(f, "{indent_string}case ")?;
            fmt_args(f, exprs)?;
            writeln!(f)?;

            writeln!(f, "{indent_string}{{")?;

            if stmts.len() > 0 {
                let pretty_body = PrettyStmts::with_indent(&stmts[..], indent + 1);
                writeln!(f, "{pretty_body}")?;
            }

            write!(f, "{indent_string}}}")?;
        }

        SwitchCase::Default(stmts) => {
            writeln!(f, "{indent_string}default")?;
            writeln!(f, "{indent_string}{{")?;

            let pretty_body = PrettyStmts::with_indent(&stmts[..], indent + 1);
            writeln!(f, "{pretty_body}")?;

            write!(f, "{indent_string}}}")?;
        }
    })
}
