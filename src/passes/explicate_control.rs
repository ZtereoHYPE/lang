use std::clone;
use crate::states::ast::{Identifier, Literal};
use crate::states::ir::{Assignment, Atom, Terminal};
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

#[derive(Clone)]
enum Tail {
    Assign(ir::Assignment, Box<Tail>),
    Term(ir::Terminal),
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

        let tail = self.explicate_tail(function.body);
        let entrypoint = self.create_block(tail);

        ir::Function {
            name: function.id,
            entrypoint,
            params: function.params.into_iter().map(|(i, _)| i).collect(),
            blocks: std::mem::take(&mut self.blocks), // this also clears self.blocks, ready for the next function. Neat!
        }
    }

    const RETURN_UNIT: Tail = Tail::Term(ir::Terminal::Return(ir::Expression::Atom(ir::Atom::Value(Literal::Unit()))));

    fn explicate_tail(&mut self, expr: ast::Expression) -> Tail {
        match expr {
            ast::Expression::Block { statements, expression, .. } => {
                statements
                    .into_iter()
                    .rev()
                    .fold(
                        self.explicate_tail(*expression.unwrap()),
                        |a, s| self.explicate_statement(s, a)
                    )
            }

            // only instance of code duplication: both the branches of the if statement return the expression!
            ast::Expression::If { expression, then, else_expr } => {
                let then_label = {
                    let tail = self.explicate_tail(*then);
                    self.create_block(tail)
                };

                let else_label = if let Some(else_expr) = else_expr {
                    let tail = self.explicate_tail(*else_expr);
                    self.create_block(tail)
                } else {
                    self.create_block(Self::RETURN_UNIT)
                };

                Tail::Term(Terminal::Conditional{
                    condition: atom_of(*expression),
                    then_label,
                    else_label
                })
            }

            ast::Expression::While { expression, block } => {
                let condition_label = self.block_label();

                let then_label = {
                    let tmp_id = self.tmp_id();
                    let tail = self.explicate_assignment(*block, tmp_id, Tail::Term(Terminal::Goto {label: condition_label.clone()}));
                    self.create_block(tail)
                };

                let else_label = self.create_block(Self::RETURN_UNIT);

                self.create_named_block(
                    Tail::Term(Terminal::Conditional {
                        condition: atom_of(*expression),
                        then_label,
                        else_label
                    }),
                    condition_label.clone()
                );

                Tail::Term(Terminal::Goto {label: condition_label})
            }

            e => Tail::Term(Terminal::Return(expr_of(e)))
        }
    }

    fn explicate_assignment(&mut self, expr: ast::Expression, id: Identifier, cont: Tail) -> Tail {
        match expr {
            ast::Expression::Block { statements, expression, .. } => {
                statements
                    .into_iter()
                    .fold(
                        self.explicate_assignment(*expression.unwrap(), id, cont),
                        |t, s| self.explicate_statement(s, t)
                    )
            }

            ast::Expression::If { expression, then, else_expr } => {
                let after_label = self.create_block(cont);

                let then_label = {
                    let tail = self.explicate_assignment(*then, id.clone(), Tail::Term(Terminal::Goto { label: after_label.clone() }));
                    self.create_block(tail)
                };

                let else_label = if let Some(else_expr) = else_expr {
                    let tail = self.explicate_assignment(*else_expr, id, Tail::Term(Terminal::Goto { label: after_label }));
                    self.create_block(tail)
                } else {
                    after_label
                };

                Tail::Term(Terminal::Conditional{
                    condition: atom_of(*expression),
                    then_label,
                    else_label
                })
            }

            ast::Expression::While { expression, block } => {
                let condition_label = self.block_label();

                let then_label = {
                    let tmp_id = self.tmp_id();
                    let tail = self.explicate_assignment(*block, tmp_id, Tail::Term(Terminal::Goto {label: condition_label.clone()}));
                    self.create_block(tail)
                };

                let else_label = {
                    let tail = self.explicate_assignment(ast::Expression::Literal(Literal::Unit()), id, cont);
                    self.create_block(tail)
                };

                self.create_named_block(
                    Tail::Term(Terminal::Conditional {
                        condition: atom_of(*expression),
                        then_label,
                        else_label
                    }),
                    condition_label.clone()
                );

                Tail::Term(Terminal::Goto {label: condition_label})

            }

            e => Tail::Assign((id, expr_of(e)), Box::new(cont))
        }
    }

    fn explicate_statement(&mut self, statement: ast::Statement, cont: Tail) -> Tail {
        match statement {
            ast::Statement::Assignment { id, expression }
            | ast::Statement::Declaration { id, expression, .. } => {
                self.explicate_assignment(expression, id, cont)
            }

            ast::Statement::Expression(expr) => {
                let tmp_id = self.tmp_id();
                self.explicate_assignment(expr, tmp_id, cont)
            }
        }
    }

    fn create_block(&mut self, block: Tail) -> String {
        if let Tail::Term(ir::Terminal::Goto{ label }) = block {
            // This block is just a goto to somewhere, return that somewhere directly.
            return label;
        }

        let name = self.block_label();
        self.create_named_block(block, name.clone());
        name
    }

    fn create_named_block(&mut self, block: Tail, name: String) {
        let (assignments, terminal) = tail_to_ass_term(block);

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

    fn block_label(&mut self) -> String {
        self.block_idx += 1;
        format!("block_{}", self.block_idx)
    }

    fn tmp_id(&mut self) -> Identifier {
        self.tmp_idx += 1;
        Identifier {
            id: format!("_{}", self.tmp_idx),
        }
    }
}

fn tail_to_ass_term(mut tail: Tail) -> (Vec<Assignment>, Terminal) {
    let mut assignments = Vec::new();

    while let Tail::Assign(a, t) = tail {
        assignments.push(a);
        tail = *t;
    }

    let Tail::Term(terminal) = tail else { unreachable!() };

    (assignments, terminal)
}

fn expr_of(expr: ast::Expression) -> ir::Expression {
    match expr {
        ast::Expression::UnaryOp { op, expr } => ir::Expression::Unary {
            op,
            atom: atom_of(*expr),
        },

        ast::Expression::BinaryOp { lhs, op, rhs } => ir::Expression::Binary {
            lhs: atom_of(*lhs),
            op,
            rhs: atom_of(*rhs),
        },

        ast::Expression::FunctionCall { id, args } => {
            let args = args.into_iter().map(|a| atom_of(a)).collect();
            ir::Expression::FunCall { id, args }
        }

        atom => ir::Expression::Atom(atom_of(atom)),
    }
}

fn atom_of(atom: ast::Expression) -> ir::Atom {
    match atom {
        ast::Expression::Variable { id } => ir::Atom::Variable { id },
        ast::Expression::Literal(val) => ir::Atom::Value(val),

        e => panic!("Expression should be an Atom! Found {:?} instead", e),
    }
}
