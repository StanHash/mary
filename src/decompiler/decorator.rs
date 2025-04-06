use std::iter::zip;

use crate::{
    ast::{Expr, Invoke, Stmt},
    const_scope::ConstScope,
    ir::{IntValue, StrValue, ValueType},
};

use super::error::DecompileError;

struct StringDecorateVisitor<'a> {
    constant_names: Vec<String>,
    next_to_place: IntValue,
    const_scope: &'a ConstScope,
}

impl<'a> StringDecorateVisitor<'a> {
    fn new(constant_names: Vec<String>, const_scope: &'a ConstScope) -> Self {
        Self {
            const_scope,
            constant_names,
            next_to_place: 0,
        }
    }

    fn stringify_expr(&mut self, expr: &mut Expr) -> Result<(), ()> {
        match expr {
            Expr::Int(int_value) => {
                let int_value = *int_value;

                if int_value == self.next_to_place {
                    /* OK: we found where next_to_place is placed first, increment it */
                    self.next_to_place += 1;
                } else if int_value > self.next_to_place {
                    /* ERROR: we couldn't place the value we need to place, and found
                     * an instance of the one after that */
                    return Err(());
                }

                *expr = Expr::Name(self.constant_names[int_value as usize].clone());
                Ok(())
            }

            _ => Err(()),
        }
    }

    fn visit_invoke(&mut self, invoke: &mut Invoke) -> Result<(), DecompileError> {
        let (_, shape) = &self.const_scope.callable_map()[&invoke.func];

        assert_eq!(shape.num_parameters(), invoke.args.len());

        let mut error = false;

        for (expr, param_type) in zip(&mut invoke.args, shape.parameter_types()) {
            match *param_type {
                ValueType::String => {
                    if let Err(_) = self.stringify_expr(expr) {
                        error = true;
                    }
                }

                _ => {
                    if let Err(_) = self.visit_expr(expr) {
                        error = true;
                    }
                }
            }
        }

        if error {
            Err(DecompileError::NotEnoughStringConsumers)
        } else {
            Ok(())
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<(), DecompileError> {
        match expr {
            Expr::Name(_)
            | Expr::Int(_)
            | Expr::Str(_)
            | Expr::PostIncrement(_)
            | Expr::PreIncrement(_)
            | Expr::PostDecrement(_)
            | Expr::PreDecrement(_) => Ok(()),

            Expr::OpAdd(exprs)
            | Expr::OpSub(exprs)
            | Expr::OpMul(exprs)
            | Expr::OpDiv(exprs)
            | Expr::OpMod(exprs)
            | Expr::OpOr(exprs)
            | Expr::OpAnd(exprs)
            | Expr::CmpEq(exprs)
            | Expr::CmpNe(exprs)
            | Expr::CmpLt(exprs)
            | Expr::CmpLe(exprs)
            | Expr::CmpGe(exprs)
            | Expr::CmpGt(exprs) => {
                self.visit_expr(&mut exprs.0)?;
                self.visit_expr(&mut exprs.1)
            }

            Expr::OpNeg(expr) | Expr::OpNot(expr) => self.visit_expr(expr),

            Expr::Call(invoke) => self.visit_invoke(invoke),
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Result<(), DecompileError> {
        match stmt {
            Stmt::Vars(items) => {
                for (_, option_expr) in items {
                    if let Some(expr) = option_expr {
                        self.visit_expr(expr)?;
                    }
                }

                Ok(())
            }

            Stmt::Consts(items) => {
                for (_, expr) in items {
                    self.visit_expr(expr)?;
                }

                Ok(())
            }

            Stmt::Assign(_, _, expr) => self.visit_expr(expr),
            Stmt::Expr(expr) => self.visit_expr(expr),

            Stmt::Call(invoke) => self.visit_invoke(invoke),

            Stmt::If(expr, stmts) => {
                self.visit_expr(expr)?;
                self.visit_stmts(stmts)
            }

            Stmt::IfElse(expr, stmts_then, stmts_else) => {
                self.visit_expr(expr)?;
                self.visit_stmts(stmts_then)?;
                self.visit_stmts(stmts_else)
            }

            Stmt::For(for_elems) => {
                self.visit_stmt(&mut for_elems.1)?;
                self.visit_expr(&mut for_elems.0)?;
                self.visit_stmt(&mut for_elems.2)?;
                self.visit_stmts(&mut for_elems.3)
            }

            Stmt::DoWhile(expr, stmts) => {
                self.visit_expr(expr)?;
                self.visit_stmts(stmts)
            }

            Stmt::Switch(expr, switch_cases, _) => {
                self.visit_expr(expr)?;

                for switch_case in switch_cases {
                    self.visit_stmts(switch_case.stmts_mut())?;
                }

                Ok(())
            }

            Stmt::Exit => Ok(()),
        }
    }

    fn visit_stmts(&mut self, stmts: &mut [Stmt]) -> Result<(), DecompileError> {
        let mut result = Ok(());

        for stmt in stmts {
            if let Err(err) = self.visit_stmt(stmt) {
                result = Err(err);
            }
        }

        result
    }
}

pub(super) fn decorate_stmts_with_strings(
    stmts: &mut Vec<Stmt>,
    strings: &[StrValue],
    const_scope: &ConstScope,
) -> Result<(), DecompileError> {
    let mut string_constants = vec![];
    let mut string_constant_stmts = vec![];

    for i in 0..strings.len() {
        let string = &strings[i];
        let name = format!("MESSAGE_{i}");

        string_constants.push(name.clone());
        string_constant_stmts.push(Stmt::Consts(vec![(name, Expr::Str(string.clone()))]));
    }

    string_constant_stmts.append(stmts);
    *stmts = string_constant_stmts;

    let mut visitor = StringDecorateVisitor::new(string_constants, const_scope);
    visitor.visit_stmts(stmts)?;

    if (visitor.next_to_place as usize) < strings.len() {
        Err(DecompileError::NotEnoughStringConsumers)
    } else {
        Ok(())
    }
}
