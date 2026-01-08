use crate::states::ast::Identifier;
use crate::states::ir::{Assignment, Terminal};
use crate::states::{ast, ir};
use std::collections::HashMap;

pub fn explicate_control(program: ast::Program) -> ir::Program {
    let mut expl = ExplicateControl {
        block_idx: 0,
        tmp_idx: 0,
        blocks: HashMap::new(),
    };
    expl.explicate_program(program)
}

enum ExprContext {
    Return,
    Assign(Identifier),
}

// state for ExplicateControl
struct ExplicateControl {
    block_idx: usize,
    tmp_idx: usize,
    blocks: HashMap<String, ir::Block>,
}

impl ExplicateControl {
    fn explicate_program(&mut self, program: ast::Program) -> ir::Program {
        ir::Program {
            functions: program
                .functions
                .into_iter()
                .map(|i| self.explicate_function(i))
                .collect(),
        }
    }

    fn explicate_function(&mut self, function: ast::Function) -> ir::Function {
        self.block_idx = 0;

        let first_block_name = format! {"block_{}", self.block_idx};

        // todo: clean this up
        let (ass, term) = self.explicate(function.body, ExprContext::Return);

        let name = self.block_label();
        self.blocks.insert(
            name.clone(),
            ir::Block {
                name: name.clone(),
                liveness: vec![],
                assignments: ass,
                terminal: term.unwrap(),
            },
        );

        ir::Function {
            name: function.id,
            entrypoint: first_block_name,
            params: function.params.into_iter().map(|(i, _)| i).collect(),
            blocks: std::mem::take(&mut self.blocks), // this also clears self.blocks, ready for the next function. Neat!
        }
    }

    fn explicate(
        &mut self,
        expr: ast::Expression,
        ctx: ExprContext,
    ) -> (Vec<Assignment>, Option<Terminal>) {
        match expr {
            ast::Expression::UnaryOp { .. }
            | ast::Expression::BinaryOp { .. }
            | ast::Expression::FunctionCall { .. }
            | ast::Expression::Variable { .. }
            | ast::Expression::Literal(_) => {
                let expr = self.explicate_expression(expr);
                match ctx {
                    ExprContext::Return => (vec![], Some(Terminal::Return(expr))),
                    ExprContext::Assign(x) => (vec![(x, expr)], None),
                }
            }

            ast::Expression::Block {
                statements,
                expression,
                ..
            } => {
                let mut stats = statements
                    .into_iter()
                    .flat_map(|s| self.explicate_statement(s))
                    .collect::<Vec<_>>();

                let (mut ass, term) = self.explicate(*expression.unwrap(), ctx);
                stats.append(&mut ass);

                (stats, term)
            }

            ast::Expression::If {
                expression,
                then,
                else_expr,
            } => {
                // INITIAL BLOCK
                let condition = self.explicate_atom(*expression);
                // todo: this is not nice. We are returning the last block such that gotos can be appended to it by create_block but it means that the statements from the parent block case dont wokr

                // BRANCH BLOCKS
                let final_label = self.block_label();
                let ass_id = match ctx {
                    ExprContext::Return => self.tmp_id(),
                    ExprContext::Assign(id) => id,
                };

                let (t_ass, _) = self.explicate(*then, ExprContext::Assign(ass_id));
                let (e_ass, _) = self.explicate(*else_expr.unwrap(), ExprContext::Assign(ass_id)); // todo: we are lying
                self.create_block(t_ass, ir::Terminal::Goto { label: final_label });
                self.create_block(e_ass, ir::Terminal::Goto { label: final_label });

                // FINAL BLOCK
                match ctx {
                    ExprContext::Return => (
                        vec![],
                        Some(Terminal::Return(ir::Expression::Atom(ir::Atom::Variable {
                            id: ass_id,
                        }))),
                    ),
                    ExprContext::Assign(x) => (vec![], None),
                }
            }

            ast::Expression::While { .. } => {
                // todo
            }
        }
    }

    fn explicate_expression(&mut self, expr: ast::Expression) -> ir::Expression {
        match expr {
            ast::Expression::UnaryOp { op, expr } => ir::Expression::Unary {
                op,
                atom: self.explicate_atom(*expr),
            },

            ast::Expression::BinaryOp { lhs, op, rhs } => ir::Expression::Binary {
                lhs: self.explicate_atom(*lhs),
                op,
                rhs: self.explicate_atom(*rhs),
            },

            ast::Expression::FunctionCall { id, args } => {
                let args = args.into_iter().map(|a| self.explicate_atom(a)).collect();
                ir::Expression::FunCall { id, args }
            }

            atom => ir::Expression::Atom(self.explicate_atom(atom)),
        }
    }

    fn explicate_atom(&mut self, atom: ast::Expression) -> ir::Atom {
        match atom {
            ast::Expression::Variable { id } => ir::Atom::Variable { id },
            ast::Expression::Literal(val) => ir::Atom::Value(val),

            e => panic!("Expression should be an Atom! Found {:?} instead", e),
        }
    }

    fn explicate_statement(&mut self, statement: ast::Statement) -> Vec<ir::Assignment> {
        match statement {
            ast::Statement::Assignment { id, expression }
            | ast::Statement::Declaration { id, expression, .. } => {
                let (ass, _) = self.explicate(expression, ExprContext::Assign(id));
                ass
            }

            ast::Statement::Expression(expr) => {
                let tmp_id = self.tmp_id();
                let (ass, _) = self.explicate(expr, ExprContext::Assign(tmp_id));
                ass
            }
        }
    }

    fn create_block(
        &mut self,
        assignments: Vec<(Identifier, ir::Expression)>,
        terminal: ir::Terminal,
    ) {
        // // This block is just a goto to somewhere, return that somewhere directly.
        // if let ir::Terminal::Goto{ label } = &terminal && assignments.is_empty() {
        //     return;
        // }

        let name = self.block_label();
        self.blocks.insert(
            name.clone(),
            ir::Block {
                name,
                liveness: vec![],
                assignments,
                terminal,
            },
        );
    }

    pub fn block_label(&mut self) -> String {
        self.block_idx += 1;
        format!("block_{}", self.block_idx)
    }

    pub fn tmp_id(&mut self) -> Identifier {
        self.tmp_idx += 1;
        Identifier {
            id: format!("_{}", self.tmp_idx),
        }
    }
}
