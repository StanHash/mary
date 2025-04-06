mod decorator;
mod error;
mod ins_decompiler;
mod state;

use std::collections::HashMap;

pub use error::{DecompileError, DecompileErrorExtra};
use ins_decompiler::decompile_instructions;
use state::DecompileState;
use state::DecompileToken;

use crate::{ast::Stmt, const_scope::ConstScope, ir::Script};

pub fn decompile_script<'a>(
    script: &'a Script,
    const_scope: &ConstScope,
) -> Result<Vec<Stmt>, DecompileErrorExtra<'a>> {
    let mut known_callables = HashMap::new();

    let callable_map = const_scope.callable_map();

    for (name, (call_id, shape)) in callable_map {
        known_callables.insert(*call_id, (name.clone(), shape.clone()));
    }

    let mut stmts = decompile_instructions(&script.instructions, &known_callables)?;

    match decorator::decorate_stmts_with_strings(&mut stmts, &script.strings, const_scope) {
        Ok(()) => {}
        Err(err) => return Err(DecompileErrorExtra(err, stmts.into())),
    }

    // TODO: decorate
    Ok(stmts)
}

#[cfg(test)]
mod tests {
    use crate::ast::{AssignOperation, Expr, Invoke, Stmt};
    use crate::bytecode;
    use crate::ir::{CallId, Ins, JumpId, VarId};

    use super::*;

    #[test]
    fn test_decompile() {
        let mut const_sope = ConstScope::new();

        const_sope.add_proc("FakeProcedure".to_string(), CallId(2), vec![]);

        let var_0 = "var_0".to_string();

        let scripts = [(
            Script {
                instructions: vec![
                    Ins::PushInt(0),
                    Ins::PushInt(10),
                    Ins::Assign,
                    Ins::Discard,
                    Ins::Label(JumpId(2)),
                    Ins::Call(CallId(2)),
                    Ins::PushVar(VarId(0)),
                    Ins::Dec,
                    Ins::Dupe,
                    Ins::PopVar(VarId(0)),
                    Ins::Discard,
                    Ins::PushVar(VarId(0)),
                    Ins::PushInt(0),
                    Ins::Cmp,
                    Ins::Bne(JumpId(0)),
                    Ins::PushInt(0),
                    Ins::Jmp(JumpId(1)),
                    Ins::Label(JumpId(0)),
                    Ins::PushInt(1),
                    Ins::Label(JumpId(1)),
                    Ins::Bne(JumpId(2)),
                ],
                strings: vec![],
            },
            vec![
                // TODO: declare variables
                Stmt::Vars(vec![(var_0.clone(), None)]),
                Stmt::Assign(AssignOperation::None, var_0.clone(), Expr::Int(10)),
                Stmt::DoWhile(
                    Expr::CmpNe(Box::new((Expr::Name(var_0.clone()), Expr::Int(0)))),
                    vec![
                        Stmt::Call(Invoke {
                            func: "FakeProcedure".to_string(),
                            args: vec![],
                        }),
                        Stmt::Expr(Expr::PreDecrement(var_0.clone())),
                    ],
                ),
            ],
        )];

        for (script, expected_decompiled) in scripts {
            let encoded = bytecode::encode_script(&script);
            let decoded = bytecode::decode_script(&mut &encoded[..]).unwrap();

            let decompiled = decompile_script(&decoded, &const_sope).unwrap();

            assert_eq!(&decompiled[..], &expected_decompiled[..]);
        }
    }
}
