use crate::{
    ast::{Stmt, SwitchCase},
    ir::SwitchId,
};

#[derive(Debug, Default)]
struct SwitchAllocator {
    switch_counter: usize,
}

impl SwitchAllocator {
    fn new_switch(&mut self) -> SwitchId {
        self.switch_counter = self.switch_counter + 1;
        SwitchId(self.switch_counter - 1)
    }

    fn visit_switch_cases(&mut self, switch_cases: &mut [SwitchCase]) {
        for switch_case in switch_cases {
            match switch_case {
                SwitchCase::Case(_, stmts) => self.visit_stmts(stmts),
                SwitchCase::Default(stmts) => self.visit_stmts(stmts),
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::If(_, stmts) => self.visit_stmts(stmts),

            Stmt::IfElse(_, stmts, stmts1) => {
                self.visit_stmts(stmts);
                self.visit_stmts(stmts1);
            }

            Stmt::For(params) => {
                self.visit_stmt(&mut params.1);
                self.visit_stmt(&mut params.2);
                self.visit_stmts(&mut params.3);
            }

            Stmt::DoWhile(_, stmts) => self.visit_stmts(stmts),

            Stmt::Switch(_, switch_cases, switch_id) => {
                /* IMPORTANT FOR MATCHING: depth-first */
                self.visit_switch_cases(switch_cases);
                *switch_id = self.new_switch();
            }

            Stmt::Vars(_)
            | Stmt::Consts(_)
            | Stmt::Assign(_, _, _)
            | Stmt::Expr(_)
            | Stmt::Call(_)
            | Stmt::Exit => {}
        }
    }

    fn visit_stmts(&mut self, stmts: &mut [Stmt]) {
        for stmt in stmts {
            self.visit_stmt(stmt);
        }
    }
}

pub(super) fn allocate_switch_ids(stmts: &mut [Stmt]) {
    let mut switch_allocator = SwitchAllocator::default();
    switch_allocator.visit_stmts(stmts);
}
