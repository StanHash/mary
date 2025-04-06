use std::fmt;

use crate::{
    ast::{AssignOperation, Expr, Invoke, Stmt, SwitchCase},
    ir::{CaseEnum, Ins, IntValue, JumpId, SwitchId, VarId},
    pretty_print::{fmt_invoke, fmt_switch_case, PrettyExpr, PrettyStmts},
};

#[derive(Debug)]
pub(super) enum DecompileToken {
    /* The target */
    Stmts(Vec<Stmt>),

    /* AST bits to-be-reduced */
    Expr(Expr),
    AssignExpr(VarId, AssignOperation, Expr),
    SwitchCases(SwitchId, Vec<(SwitchCase, Option<JumpId>)>),
    FunctionCall(Invoke), // procedure calls can be reduced to statements immediately

    /* those are usually expressions but sometimes need special handling,
     * so we distinguish them */
    PushVar(VarId),
    PushInt(IntValue),

    /* input instructions present in back-patterns to match control structures */
    Beq(JumpId),
    Bne(JumpId),
    Jump(JumpId),
    Label(JumpId),
    Case(SwitchId, CaseEnum),
}

#[derive(Debug)]
pub struct DecompileState<'a> {
    pub(super) stack: Vec<DecompileToken>,
    pub(super) input: &'a [Ins],
}

impl<'a> DecompileState<'a> {
    pub(super) fn new(input: &'a [Ins]) -> Self {
        Self {
            stack: vec![],
            input,
        }
    }
}

impl<'a> From<Vec<Stmt>> for DecompileState<'a> {
    fn from(stmts: Vec<Stmt>) -> Self {
        Self {
            stack: vec![DecompileToken::Stmts(stmts)],
            input: &[],
        }
    }
}

impl<'a> fmt::Display for DecompileState<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let separator = "================================";

        writeln!(f, "{separator} PARTIAL DECOMPILATION")?;

        for token in &self.stack {
            match token {
                DecompileToken::Stmts(stmts) => {
                    writeln!(f, "    /* Complete statements */")?;

                    /* {stmts} */
                    let pretty_stmts = PrettyStmts::with_indent(stmts, 1);
                    writeln!(f, "{pretty_stmts}")?;
                }

                DecompileToken::Expr(expr) => {
                    writeln!(f, "    /* Expression */")?;
                    /* PUSH {expr} */
                    let pretty_expr = PrettyExpr::new(&expr);
                    writeln!(f, "    push {pretty_expr}")?;
                }

                DecompileToken::AssignExpr(var_id, assign_operation, expr) => {
                    writeln!(f, "    /* Assignment Expression */")?;
                    /* PUSH var_{0} {assign_operation} {expr} */
                    let pretty_expr = PrettyExpr::new(&expr);
                    writeln!(
                        f,
                        "    push var_{0} {1} {pretty_expr}",
                        var_id.0, assign_operation
                    )?;
                }

                DecompileToken::SwitchCases(switch_id, switch_cases) => {
                    writeln!(f, "    /* Case blocks (switch {0}) */", switch_id.0)?;

                    for (switch_case, break_jump_id) in switch_cases {
                        fmt_switch_case(f, switch_case, 1)?;

                        match break_jump_id {
                            Some(jump_id) => writeln!(f, " // breaks to location_{0}", jump_id.0),
                            None => writeln!(f, " // doesn't break (exits)"),
                        }?;
                    }
                }

                DecompileToken::FunctionCall(invoke) => {
                    writeln!(f, "    /* Function Call Expression */")?;
                    write!(f, "    push ")?;
                    fmt_invoke(f, invoke)?;
                    writeln!(f)?;
                }

                DecompileToken::PushVar(var_id) => {
                    writeln!(f, "    /* Variable Value */")?;
                    writeln!(f, "    push var_{0}", var_id.0)?;
                }

                DecompileToken::PushInt(val) => {
                    writeln!(f, "    /* Integer Value */")?;
                    writeln!(f, "    push {0}", val)?;
                }

                DecompileToken::Beq(jump_id) => {
                    writeln!(f, "    beq location_{0}", jump_id.0)?;
                }

                DecompileToken::Bne(jump_id) => {
                    writeln!(f, "    bne location_{0}", jump_id.0)?;
                }

                DecompileToken::Jump(jump_id) => {
                    writeln!(f, "    jmp location_{0}", jump_id.0)?;
                }

                DecompileToken::Label(jump_id) => {
                    writeln!(f, "location_{0}:", jump_id.0)?;
                }

                DecompileToken::Case(_, CaseEnum::Default) => {
                    writeln!(f, "default:")?;
                }

                DecompileToken::Case(_, CaseEnum::Val(case_value)) => {
                    writeln!(f, "case {case_value}:")?;
                }
            }
        }

        if self.input.len() > 0 {
            writeln!(f, "{separator} REMAINING INSTRUCTIONS")?;

            writeln!(f, ">   {0:?}", self.input[0])?;

            for instruction in &self.input[1..] {
                writeln!(f, "    {instruction:?}")?;
            }
        }

        write!(f, "{separator} END OF SCRIPT")
    }
}
