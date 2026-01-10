use crate::states::ast::{Identifier, Literal};
use crate::states::ir::{Assignment, Atom, Terminal};
use crate::states::{ast, ir};
use std::clone;
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

                Tail::Term(Terminal::Conditional {
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

// add tests for atom_of, expr_of, tail_to_ass_term
#[cfg(test)]
mod tests {
    use super::*;
    use crate::states::ast::{
        Expression, Function as AstFunction, Literal, Operator, Program as AstProgram, SymbolTable,
        Type,
    };
    use std::collections::HashMap;

    fn id(name: &str) -> Identifier {
        Identifier { id: name.into() }
    }

    #[test]
    fn atom_of_variable() {
        let a = atom_of(Expression::Variable { id: id("x") });
        match a {
            ir::Atom::Variable { id } => assert_eq!(id.id, "x"),
            _ => panic!("expected variable atom"),
        }
    }

    #[test]
    fn atom_of_literal() {
        let a = atom_of(Expression::Literal(Literal::Int(5)));
        match a {
            ir::Atom::Value(Literal::Int(v)) => assert_eq!(v, 5),
            _ => panic!("expected int literal atom"),
        }
    }

    #[test]
    #[should_panic]
    fn atom_of_non_atom_panics() {
        atom_of(Expression::UnaryOp {
            op: Operator::Negated,
            expr: Box::new(Expression::Literal(Literal::Int(1))),
        });
    }

    #[test]
    fn expr_of_unary_and_binary() {
        let e_un = expr_of(Expression::UnaryOp {
            op: Operator::Negated,
            expr: Box::new(Expression::Variable { id: id("x") }),
        });
        match e_un {
            ir::Expression::Unary { op, atom } => {
                assert!(matches!(op, Operator::Negated));
                assert!(matches!(atom, ir::Atom::Variable { .. }));
            }
            _ => panic!("expected unary expression"),
        }

        let e_bin = expr_of(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Plus,
            rhs: Box::new(Expression::Variable { id: id("y") }),
        });
        match e_bin {
            ir::Expression::Binary { lhs, op, rhs } => {
                assert!(matches!(lhs, ir::Atom::Value(Literal::Int(1))));
                assert!(matches!(op, Operator::Plus));
                assert!(matches!(rhs, ir::Atom::Variable { .. }));
            }
            _ => panic!("expected binary expression"),
        }
    }

    #[test]
    fn expr_of_function_call_and_atom() {
        let e = expr_of(Expression::FunctionCall {
            id: id("f"),
            args: vec![
                Expression::Literal(Literal::Int(2)),
                Expression::Variable { id: id("z") },
            ],
        });
        match e {
            ir::Expression::FunCall { id, args } => {
                assert_eq!(id.id, "f");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], ir::Atom::Value(Literal::Int(2))));
                assert!(matches!(args[1], ir::Atom::Variable { .. }));
            }
            _ => panic!("expected function call expression"),
        }

        let a = expr_of(Expression::Variable { id: id("x") });
        assert!(matches!(a, ir::Expression::Atom(ir::Atom::Variable { .. })));
    }

    #[test]
    fn tail_to_ass_term_collects_assignments_and_terminal() {
        let a_id = id("a");
        let b_id = id("b");

        let tail = Tail::Assign(
            (a_id.clone(), expr_of(Expression::Literal(Literal::Int(1)))),
            Box::new(Tail::Assign(
                (
                    b_id.clone(),
                    expr_of(Expression::Variable { id: a_id.clone() }),
                ),
                Box::new(Tail::Term(ir::Terminal::Return(ir::Expression::Atom(
                    ir::Atom::Variable { id: b_id.clone() },
                )))),
            )),
        );

        let (assignments, terminal) = tail_to_ass_term(tail);
        assert_eq!(assignments.len(), 2);
        assert_eq!(assignments[0].0.id, "a");
        assert_eq!(assignments[1].0.id, "b");

        match terminal {
            ir::Terminal::Return(ir::Expression::Atom(ir::Atom::Variable { id })) => {
                assert_eq!(id.id, "b")
            }
            _ => panic!("expected return of variable b"),
        }
    }

    #[test]
    fn explicate_simple_block_returns_binary() {
        let func = AstFunction {
            id: id("main"),
            symbols: SymbolTable::new(),
            params: vec![],
            ty: Type::Int,
            body: Expression::Block {
                symbols: SymbolTable::new(),
                statements: vec![ast::Statement::Declaration {
                    id: id("x"),
                    ty: Type::Int,
                    expression: Expression::Literal(Literal::Int(10)),
                }],
                expression: Some(Box::new(Expression::BinaryOp {
                    lhs: Box::new(Expression::Variable { id: id("x") }),
                    op: Operator::Minus,
                    rhs: Box::new(Expression::Literal(Literal::Int(5))),
                })),
            },
        };

        let program = AstProgram {
            symbols: HashMap::new(),
            functions: vec![func],
        };

        let ir = explicate_control(program);
        assert_eq!(ir.functions.len(), 1);

        let f = &ir.functions[0];
        assert_eq!(f.entrypoint, "block_1");
        assert_eq!(f.blocks.len(), 1);

        let block = f.blocks.get(&f.entrypoint).expect("entry block exists");
        assert_eq!(block.assignments.len(), 1);
        assert_eq!(block.assignments[0].0.id, "x");
        assert!(matches!(
            block.assignments[0].1,
            ir::Expression::Atom(ir::Atom::Value(Literal::Int(10)))
        ));

        match &block.terminal {
            ir::Terminal::Return(ir::Expression::Binary { lhs, op, rhs }) => {
                assert!(matches!(lhs, ir::Atom::Variable { id } if id.id == "x"));
                assert!(matches!(op, Operator::Minus));
                assert!(matches!(rhs, ir::Atom::Value(Literal::Int(5))));
            }
            _ => panic!("expected return of binary expression"),
        }
    }

    #[test]
    fn explicate_if_only_function_returns_branches() {
        let func = AstFunction {
            id: id("if_only"),
            symbols: SymbolTable::new(),
            params: vec![],
            ty: Type::Int,
            body: Expression::If {
                expression: Box::new(Expression::Literal(Literal::Bool(true))),
                then: Box::new(Expression::Literal(Literal::Int(1))),
                else_expr: Some(Box::new(Expression::UnaryOp {
                    op: Operator::Negated,
                    expr: Box::new(Expression::Literal(Literal::Int(1))),
                })),
            },
        };

        let program = AstProgram {
            symbols: HashMap::new(),
            functions: vec![func],
        };

        let ir = explicate_control(program);
        assert_eq!(ir.functions.len(), 1);

        let f = &ir.functions[0];
        assert_eq!(f.blocks.len(), 3);
        assert_eq!(f.entrypoint, "block_3");

        let then_block = f.blocks.get("block_1").expect("then block");
        let else_block = f.blocks.get("block_2").expect("else block");
        let cond_block = f.blocks.get("block_3").expect("entry block");

        assert!(then_block.assignments.is_empty());
        match then_block.terminal {
            ir::Terminal::Return(ir::Expression::Atom(ir::Atom::Value(Literal::Int(v)))) => {
                assert_eq!(v, 1)
            }
            _ => panic!("expected return 1"),
        }

        assert!(else_block.assignments.is_empty());
        match &else_block.terminal {
            ir::Terminal::Return(ir::Expression::Unary { op, atom }) => {
                assert!(matches!(op, Operator::Negated));
                assert!(matches!(atom, ir::Atom::Value(Literal::Int(1))));
            }
            _ => panic!("expected return -1 as unary negation"),
        }

        match &cond_block.terminal {
            ir::Terminal::Conditional {
                condition,
                then_label,
                else_label,
            } => {
                assert_eq!(then_label, "block_1");
                assert_eq!(else_label, "block_2");
                assert!(matches!(condition, ir::Atom::Value(Literal::Bool(true))));
            }
            _ => panic!("expected conditional terminal"),
        }
    }

    #[test]
    fn explicate_block_assignment_from_if() {
        let func = AstFunction {
            id: id("main_if_assign"),
            symbols: SymbolTable::new(),
            params: vec![],
            ty: Type::Int,
            body: Expression::Block {
                symbols: SymbolTable::new(),
                statements: vec![
                    ast::Statement::Declaration {
                        id: id("x"),
                        ty: Type::Int,
                        expression: Expression::If {
                            expression: Box::new(Expression::Literal(Literal::Bool(true))),
                            then: Box::new(Expression::UnaryOp {
                                op: Operator::Negated,
                                expr: Box::new(Expression::Literal(Literal::Int(1))),
                            }),
                            else_expr: Some(Box::new(Expression::Literal(Literal::Int(1)))),
                        },
                    },
                    ast::Statement::Assignment {
                        id: id("x"),
                        expression: Expression::BinaryOp {
                            lhs: Box::new(Expression::Variable { id: id("x") }),
                            op: Operator::Times,
                            rhs: Box::new(Expression::Literal(Literal::Int(3))),
                        },
                    },
                ],
                expression: Some(Box::new(Expression::BinaryOp {
                    lhs: Box::new(Expression::Variable { id: id("x") }),
                    op: Operator::Plus,
                    rhs: Box::new(Expression::Literal(Literal::Int(2))),
                })),
            },
        };

        let program = AstProgram {
            symbols: HashMap::new(),
            functions: vec![func],
        };
        let ir = explicate_control(program);
        let f = &ir.functions[0];

        assert_eq!(f.blocks.len(), 4);
        assert_eq!(f.entrypoint, "block_4");

        let after_block = f.blocks.get("block_1").unwrap();
        assert_eq!(after_block.assignments.len(), 1);
        assert!(matches!(
            after_block.assignments[0].1,
            ir::Expression::Binary {
                lhs: ir::Atom::Variable { ref id },
                op: Operator::Times,
                rhs: ir::Atom::Value(Literal::Int(3))
            } if id.id == "x"
        ));
        assert!(matches!(
            after_block.terminal,
            ir::Terminal::Return(ir::Expression::Binary {
                lhs: ir::Atom::Variable { ref id },
                op: Operator::Plus,
                rhs: ir::Atom::Value(Literal::Int(2))
            }) if id.id == "x"
        ));

        let then_block = f.blocks.get("block_2").unwrap();
        assert_eq!(then_block.assignments.len(), 1);
        assert!(matches!(
            then_block.assignments[0].1,
            ir::Expression::Unary {
                op: Operator::Negated,
                atom: ir::Atom::Value(Literal::Int(1))
            }
        ));
        assert!(
            matches!(then_block.terminal, ir::Terminal::Goto { ref label } if label == "block_1")
        );

        let else_block = f.blocks.get("block_3").unwrap();
        assert_eq!(else_block.assignments.len(), 1);
        assert!(matches!(
            else_block.assignments[0].1,
            ir::Expression::Atom(ir::Atom::Value(Literal::Int(1)))
        ));
        assert!(
            matches!(else_block.terminal, ir::Terminal::Goto { ref label } if label == "block_1")
        );

        let cond_block = f.blocks.get("block_4").unwrap();
        assert!(
            matches!(cond_block.terminal, ir::Terminal::Conditional { ref condition, ref then_label, ref else_label }
            if matches!(condition, ir::Atom::Value(Literal::Bool(true)))
            && then_label == "block_2"
            && else_label == "block_3")
        );
    }

    #[test]
    fn explicate_beeg_complex_program() {
        let helper = AstFunction {
            id: id("helper"),
            symbols: SymbolTable::new(),
            params: vec![(id("a"), Type::Int)],
            ty: Type::Int,
            body: Expression::Variable { id: id("a") },
        };

        let main_func = AstFunction {
            id: id("main_complex"),
            symbols: SymbolTable::new(),
            params: vec![],
            ty: Type::Int,
            body: Expression::Block {
                symbols: SymbolTable::new(),
                statements: vec![
                    ast::Statement::Declaration {
                        id: id("x"),
                        ty: Type::Int,
                        expression: Expression::Literal(Literal::Int(1)),
                    },
                    ast::Statement::Assignment {
                        id: id("x"),
                        expression: Expression::BinaryOp {
                            lhs: Box::new(Expression::Variable { id: id("x") }),
                            op: Operator::Plus,
                            rhs: Box::new(Expression::Literal(Literal::Int(2))),
                        },
                    },
                    ast::Statement::Expression(Expression::While {
                        expression: Box::new(Expression::Literal(Literal::Bool(false))),
                        block: Box::new(Expression::Block {
                            symbols: SymbolTable::new(),
                            statements: vec![ast::Statement::Assignment {
                                id: id("x"),
                                expression: Expression::BinaryOp {
                                    lhs: Box::new(Expression::Variable { id: id("x") }),
                                    op: Operator::Times,
                                    rhs: Box::new(Expression::Literal(Literal::Int(3))),
                                },
                            }],
                            expression: Some(Box::new(Expression::Literal(Literal::Unit()))),
                        }),
                    }),
                ],
                expression: Some(Box::new(Expression::If {
                    expression: Box::new(Expression::Literal(Literal::Bool(true))),
                    then: Box::new(Expression::Variable { id: id("x") }),
                    else_expr: Some(Box::new(Expression::BinaryOp {
                        lhs: Box::new(Expression::Literal(Literal::Int(0))),
                        op: Operator::Minus,
                        rhs: Box::new(Expression::Variable { id: id("x") }),
                    })),
                })),
            },
        };

        let program = AstProgram {
            symbols: HashMap::new(),
            functions: vec![helper, main_func],
        };
        let ir = explicate_control(program);
        assert_eq!(ir.functions.len(), 2);

        let helper_ir = ir.functions.iter().find(|f| f.name.id == "helper").unwrap();
        assert_eq!(helper_ir.params.len(), 1);
        assert_eq!(helper_ir.params[0].id, "a");
        assert_eq!(helper_ir.blocks.len(), 1);
        let helper_block = helper_ir.blocks.get(&helper_ir.entrypoint).unwrap();
        assert!(helper_block.assignments.is_empty());
        assert!(
            matches!(helper_block.terminal, ir::Terminal::Return(ir::Expression::Atom(ir::Atom::Variable { ref id })) if id.id == "a")
        );

        let main_ir = ir
            .functions
            .iter()
            .find(|f| f.name.id == "main_complex")
            .unwrap();

        assert_eq!(main_ir.blocks.len(), 6);

        let entry_label = main_ir.entrypoint.clone();
        let entry = main_ir.blocks.get(&entry_label).unwrap();
        let header_label = match &entry.terminal {
            ir::Terminal::Goto { label } => label.clone(),
            _ => panic!("entry should be a goto"),
        };

        assert_eq!(entry.assignments.len(), 2);
        assert!(matches!(
            entry.assignments[0],
            (ref id, ir::Expression::Atom(ir::Atom::Value(Literal::Int(1)))) if id.id == "x"
        ));
        assert!(matches!(
            entry.assignments[1],
            (ref id_ass, ir::Expression::Binary {
                lhs: ir::Atom::Variable { ref id },
                op: Operator::Plus,
                rhs: ir::Atom::Value(Literal::Int(2))
            }) if id_ass.id == "x" && id.id == "x"
        ));

        let header = main_ir.blocks.get(&header_label).unwrap();
        let (body_label, if_cond_label) = match &header.terminal {
            ir::Terminal::Conditional {
                condition,
                then_label,
                else_label,
            } => {
                assert!(matches!(condition, ir::Atom::Value(Literal::Bool(false))));
                (then_label.clone(), else_label.clone())
            }
            _ => panic!("while header should be conditional"),
        };

        let body = main_ir.blocks.get(&body_label).unwrap();

        assert_eq!(body.assignments.len(), 2);
        assert!(matches!(
            body.assignments[0],
            (ref id_ass, ir::Expression::Binary {
                lhs: ir::Atom::Variable { ref id },
                op: Operator::Times,
                rhs: ir::Atom::Value(Literal::Int(3))
            }) if id_ass.id == "x" && id.id == "x"
        ));

        assert!(matches!(
            body.assignments[1],
            (ref id_ass, ir::Expression::Atom(Atom::Value(Literal::Unit()))) if id_ass.id.starts_with("_")
        ));

        assert!(
            matches!(body.terminal, ir::Terminal::Goto { ref label } if label == &header_label)
        );

        let if_cond = main_ir.blocks.get(&if_cond_label).unwrap();
        let (return_then_label, return_else_label) = match &if_cond.terminal {
            ir::Terminal::Conditional {
                condition,
                then_label,
                else_label,
            } => {
                assert!(matches!(condition, ir::Atom::Value(Literal::Bool(true))));
                (then_label.clone(), else_label.clone())
            }
            _ => panic!("if condition should be conditional"),
        };

        let return_then = main_ir.blocks.get(&return_then_label).unwrap();
        assert!(
            matches!(return_then.terminal, ir::Terminal::Return(ir::Expression::Atom(ir::Atom::Variable { ref id })) if id.id == "x")
        );

        let return_else = main_ir.blocks.get(&return_else_label).unwrap();
        assert!(
            matches!(return_else.terminal, ir::Terminal::Return(ir::Expression::Binary {
            lhs: ir::Atom::Value(Literal::Int(0)),
            op: Operator::Minus,
            rhs: ir::Atom::Variable { ref id }
        }) if id.id == "x")
        );
    }
}
