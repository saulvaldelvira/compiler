use hir::{Definition, HirId, Statement};
use semantic::Semantic;
use span::Span;

use super::{Eval, Execute};
use crate::{
    code_generator::CodeGenerator,
    codefuncs::{Address, Define, Metadata},
    mir::{MaplInstruction, MaplLiteral, MaplType},
};

fn get_mapl_type(id: HirId, sem: &Semantic<'_>) -> MaplType {
    let ty = sem.type_of(&id).unwrap();
    MaplType::from(ty)
}

impl Execute for Definition<'_> {
    fn execute(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        use hir::def::DefinitionKind;
        match self.kind {
            DefinitionKind::Variable { init, .. } => {
                if let Some(init) = init {
                    let ty = get_mapl_type(self.id, cg.sem);
                    MaplInstruction::Compose(Box::new([
                        self.address(cg),
                        init.eval(cg),
                        MaplInstruction::Store(ty),
                    ]))
                } else {
                    MaplInstruction::Empty
                }
            }
            DefinitionKind::Function { .. } | DefinitionKind::Struct { .. } => {
                unreachable!("We can only execute variable definitions")
            }
        }
    }
}

impl Execute for Statement<'_> {
    #[allow(clippy::too_many_lines)]
    fn execute(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        use hir::stmt::StatementKind;
        let md = self.metadata(cg);
        let ins = match self.kind {
            StatementKind::Expr(expression) => {
                let expr = expression.eval(cg);
                let ty = cg.sem.type_of(&expression.id).unwrap();

                if ty.is_empty_type() {
                    expr
                } else {
                    let ty = MaplType::from(ty);
                    MaplInstruction::Compose(Box::new([expr, MaplInstruction::Pop(ty)]))
                }
            }
            StatementKind::Block(statements) => {
                let block = statements.iter().map(|stmt| stmt.execute(cg)).collect();
                MaplInstruction::Compose(block)
            }
            StatementKind::If {
                cond,
                if_true,
                if_false,
            } => {
                let else_label = cg.next_label();
                let end_label = cg.next_label();
                MaplInstruction::Compose(Box::new([
                    cond.eval(cg),
                    MaplInstruction::Jz(else_label.clone()),
                    if_true.execute(cg),
                    MaplInstruction::Jmp(end_label.clone()),
                    MaplInstruction::DefineLabel(else_label),
                    if_false
                        .map_or(MaplInstruction::Empty, |i| i.execute(cg)),
                    MaplInstruction::DefineLabel(end_label),
                ]))
            }
            StatementKind::While { cond, body } => {
                let cond_label = cg.next_label();
                let end_label = cg.next_label();
                MaplInstruction::Compose(Box::new([
                    MaplInstruction::DefineLabel(cond_label.clone()),
                    cond.eval(cg),
                    MaplInstruction::Jz(end_label.clone()),
                    body.execute(cg),
                    MaplInstruction::Jmp(cond_label),
                    MaplInstruction::DefineLabel(end_label),
                ]))
            }
            StatementKind::For {
                init,
                cond,
                inc,
                body,
            } => {
                let mut ins = Vec::new();
                let cond_label = cg.next_label();
                let end_label = cg.next_label();

                if let Some(init) = init {
                    ins.push(init.execute(cg));
                }
                ins.push(MaplInstruction::DefineLabel(cond_label.clone()));
                if let Some(cond) = cond {
                    ins.push(cond.eval(cg));
                } else {
                    ins.push(MaplInstruction::Push(MaplLiteral::Int(1)));
                }
                ins.push(MaplInstruction::Jz(end_label.clone()));

                ins.push(body.execute(cg));
                if let Some(inc) = inc {
                    let inc = Statement::new(StatementKind::Expr(inc), Span::new());
                    ins.push(inc.execute(cg));
                }
                ins.push(MaplInstruction::Jmp(cond_label));
                ins.push(MaplInstruction::DefineLabel(end_label));

                MaplInstruction::Compose(ins.into_boxed_slice())
            }
            StatementKind::Empty => MaplInstruction::Empty,
            StatementKind::Break => todo!(),
            StatementKind::Continue => todo!(),
            StatementKind::Return(expression) => {
                let f = cg.current_function().unwrap();
                let ret = MaplInstruction::Return {
                    locals: f.locals_size,
                    params: f.params_size,
                    ret_size: f.ret_size,
                };
                if let Some(expr) = expression {
                    MaplInstruction::Compose(Box::new([expr.eval(cg), ret]))
                } else {
                    ret
                }
            }
            StatementKind::Print(expression) => {
                let ty = cg.sem.type_of(&expression.id).unwrap();
                let ty = MaplType::from(ty);
                MaplInstruction::Compose(Box::new([expression.eval(cg), MaplInstruction::Out(ty)]))
            }
            StatementKind::Read(expr) => {
                let ty = cg.sem.type_of(&expr.id).unwrap();
                let ty = MaplType::from(ty);
                MaplInstruction::Compose(Box::new([
                    expr.address(cg),
                    MaplInstruction::In(ty),
                    MaplInstruction::Store(ty),
                ]))
            }
            StatementKind::Def(definition) => definition.define(cg),
        };

        MaplInstruction::Compose(Box::new([md, ins]))
    }
}
