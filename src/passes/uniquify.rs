use std::collections::HashMap;
use crate::states::ast::{Expression, Program, Statement, Identifier};

// warning: symbol table is bad after this. oh, well.
pub fn uniquify_program(ast: &mut Program) {
    let mut uniq = Uniquify { names: HashMap::new() };
    uniq.uniquify_program(ast);
}

struct Uniquify {
    names: HashMap<String, u64>,
}

impl Uniquify {
    fn uniquify_program(&mut self, program: &mut Program) {
        for function in &mut program.functions {
            let mut names = HashMap::new();

            for (id, _) in &mut function.params {
                let new_id = self.gensym(&id);
                names.insert(id.clone(), new_id.clone());
                *id = new_id;
            }

            self.uniquify_expression(&mut function.body, names);
        }
    }

    fn uniquify_expression(&mut self, expression: &mut Expression, mut names: HashMap<Identifier, Identifier>) {
        match expression {
            Expression::If { expression, then, else_expr } => {
                self.uniquify_expression(expression, names.clone());
                self.uniquify_expression(then, names.clone());
                if let Some(else_expr) = else_expr {
                    self.uniquify_expression(else_expr, names);
                }
            }
            
            Expression::UnaryOp { expr, .. } => {
                self.uniquify_expression(expr, names)
            },
            
            Expression::BinaryOp { lhs, rhs, .. } => {
                self.uniquify_expression(lhs, names.clone());
                self.uniquify_expression(rhs, names);
            }
            
            Expression::FunctionCall { args, ..} => {
                args
                    .iter_mut()
                    .for_each(|arg| self.uniquify_expression(arg, names.clone()));
            }
            
            Expression::Variable { id } => {
                if names.contains_key(&id) {
                    *id = names[&id].clone();
                }
            }
            
            Expression::While { expression, block } => {
                self.uniquify_expression(expression, names.clone());
                self.uniquify_expression(block, names);
            }
            
            Expression::Block { statements, expression, .. } => {
                for s in statements {
                    match s {
                        Statement::Declaration { id, expression, .. } |
                        Statement::Assignment { id, expression } => {
                            let new_id = self.gensym(&id);
                            self.uniquify_expression(expression, names.clone());
                            names.insert(id.clone(), new_id.clone());
                            *id = new_id;
                        }
                        
                        Statement::Expression(e) => {
                            self.uniquify_expression(e, names.clone());
                        }
                    }
                }

                if let Some(expression) = expression {
                    self.uniquify_expression(expression, names);
                }
            }
            
            _ => {}
        }
    }

    fn gensym(&mut self, name: &Identifier) -> Identifier {
        let idx = *self.names.entry(name.id.clone()).or_insert(0);
        self.names.insert(name.id.clone(), idx + 1);

        Identifier { id: format!("{}_{}", name.id, idx) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::states::ast::{ Expression, Literal, Operator, SymbolTable, Type };
    use std::collections::HashMap;

    #[test]
    fn test_uniquify_variable() {
        let mut var = Expression::Variable { id: Identifier { id: "foo".to_string() } };
        let mut names = HashMap::new();
        names.insert(Identifier { id: "foo".to_string() }, Identifier { id: "foo_0".to_string() });
            
        let mut uniquify = Uniquify { names: HashMap::new() };
        uniquify.uniquify_expression(&mut var, names);
            
        if let Expression::Variable { id } = var {
            assert_eq!(id.id, "foo_0");
        }
    }

        #[test]
    fn test_gensym() {
        let mut uniquify = Uniquify { names: HashMap::new() };
        let id = Identifier { id: "x".to_string() };
            
        let sym1 = uniquify.gensym(&id);
        let sym2 = uniquify.gensym(&id);
        let sym3 = uniquify.gensym(&id);
            
        assert_eq!(sym1.id, "x_0");
        assert_eq!(sym2.id, "x_1");
        assert_eq!(sym3.id, "x_2");
    }

    #[test]
    fn test_binary_op() {
        let mut expr = Expression::BinaryOp {
            lhs: Box::new(Expression::Variable { id: Identifier { id: "a".to_string() } }),
            rhs: Box::new(Expression::Variable { id: Identifier { id: "b".to_string() } }),
            op: Operator::And,
        };
            
        let mut names = HashMap::new();
        names.insert(Identifier { id: "a".to_string() }, Identifier { id: "a_0".to_string() });
        names.insert(Identifier { id: "b".to_string() }, Identifier { id: "b_0".to_string() });
        
        let mut uniquify = Uniquify { names: HashMap::new() };
        uniquify.uniquify_expression(&mut expr, names);
    }

    #[test]
    fn test_block_declaration() {
        let mut expr = Expression::Block {
            statements: vec![Statement::Declaration {
                id: Identifier { id: "x".to_string() },
                expression: Expression::Literal(Literal::Int(42)),
                ty: Type::Int,
            }],
            expression: Some(Box::new(Expression::Variable { id: Identifier { id: "x".to_string() } })),
            symbols: SymbolTable::new(),
        };
            
        let mut uniquify = Uniquify { names: HashMap::new() };
        uniquify.uniquify_expression(&mut expr, HashMap::new());

        
        if let Expression::Block { statements, expression, .. } = expr {
            assert_eq!(statements.len(), 1);
            
            if let Statement::Declaration { id, .. } = &statements[0] {
                assert_eq!(id.id, "x_0");
            }
            
            if let Some(expr) = expression {
                if let Expression::Variable { id } = *expr {
                    assert_eq!(id.id, "x_0");
                }
            }
        }
    }

    #[test]
    fn test_block_multiple_declarations() {
        let mut expr = Expression::Block {
            statements: vec![
                Statement::Declaration {
                    id: Identifier { id: "x".to_string() },
                    expression: Expression::Literal(Literal::Int(10)),
                    ty: Type::Int,
                },
                Statement::Declaration {
                    id: Identifier { id: "x".to_string() },
                    expression: Expression::Literal(Literal::Int(20)),
                    ty: Type::Int,
                },
            ],
            expression: None,
            symbols: SymbolTable::new(),
        };
        
        let mut uniquify = Uniquify { names: HashMap::new() };
        uniquify.uniquify_expression(&mut expr, HashMap::new());
        
        if let Expression::Block { statements, .. } = expr {
            assert_eq!(statements.len(), 2);
            if let Statement::Declaration { id: id1, .. } = &statements[0] {
                assert_eq!(id1.id, "x_0");
            }
            if let Statement::Declaration { id: id2, .. } = &statements[1] {
                assert_eq!(id2.id, "x_1");
            }
        }
    }

    #[test]
    fn test_complex_nested_blocks_and_declarations() {
        let mut expr = Expression::Block {
            statements: vec![
                Statement::Declaration {
                    id: Identifier { id: "variable".to_string() },
                    expression: Expression::BinaryOp {
                        lhs: Box::new(Expression::Literal(Literal::Int(123))),
                        rhs: Box::new(Expression::BinaryOp {
                            lhs: Box::new(Expression::UnaryOp {
                                expr: Box::new(Expression::Literal(Literal::Int(3))),
                                op: Operator::Negated,
                            }),
                            rhs: Box::new(Expression::Block {
                                statements: vec![],
                                expression: Some(Box::new(Expression::BinaryOp {
                                    lhs: Box::new(Expression::Literal(Literal::Int(3))),
                                    rhs: Box::new(Expression::Literal(Literal::Int(4))),
                                    op: Operator::Plus,
                                })),
                                symbols: SymbolTable::new(),
                            }),
                            op: Operator::Divided,
                        }),
                        op: Operator::Plus,
                    },
                    ty: Type::Int,
                },
                Statement::Declaration {
                    id: Identifier { id: "myThing".to_string() },
                    expression: Expression::Block {
                        statements: vec![
                            Statement::Declaration {
                                id: Identifier { id: "hello".to_string() },
                                expression: Expression::Block {
                                    statements: vec![],
                                    expression: None,
                                    symbols: SymbolTable::new(),
                                },
                                ty: Type::Unit,
                            },
                            Statement::Declaration {
                                id: Identifier { id: "x".to_string() },
                                expression: Expression::Literal(Literal::Int(0)),
                                ty: Type::Int,
                            },
                            Statement::Declaration {
                                id: Identifier { id: "y".to_string() },
                                expression: Expression::Variable { id: Identifier { id: "x".to_string() } },
                                ty: Type::Int,
                            },
                            Statement::Assignment {
                                id: Identifier { id: "x".to_string() },
                                expression: Expression::Variable { id: Identifier { id: "y".to_string() } },
                            },
                            Statement::Declaration {
                                id: Identifier { id: "z".to_string() },
                                expression: Expression::BinaryOp {
                                    lhs: Box::new(Expression::Variable { id: Identifier { id: "x".to_string() } }),
                                    rhs: Box::new(Expression::Variable { id: Identifier { id: "y".to_string() } }),
                                    op: Operator::Plus,
                                },
                                ty: Type::Int,
                            },
                        ],
                        expression: Some(Box::new(Expression::BinaryOp {
                            lhs: Box::new(Expression::Variable { id: Identifier { id: "variable".to_string() } }),
                            rhs: Box::new(Expression::Literal(Literal::Int(13))),
                            op: Operator::Plus,
                        })),
                        symbols: SymbolTable::new(),
                    },
                    ty: Type::Int,
                },
            ],
            expression: None,
            symbols: SymbolTable::new(),
        };

        let mut uniquify = Uniquify { names: HashMap::new() };
        uniquify.uniquify_expression(&mut expr, HashMap::new());

        if let Expression::Block { statements, .. } = expr {
            assert_eq!(statements.len(), 2);
            if let Statement::Declaration { id, .. } = &statements[0] {
                assert_eq!(id.id, "variable_0");
            }
            if let Statement::Declaration { id, expression, .. } = &statements[1] {
                assert_eq!(id.id, "myThing_0");
                if let Expression::Block { statements: inner_stmts, .. } = &*expression {
                    if let Statement::Declaration { id: id0, .. } = &inner_stmts[0] {
                        assert_eq!(id0.id, "hello_0");
                    }
                    if let Statement::Declaration { id: id1, .. } = &inner_stmts[1] {
                        assert_eq!(id1.id, "x_0");
                    }
                    if let Statement::Declaration { id: id2, .. } = &inner_stmts[2] {
                        assert_eq!(id2.id, "y_0");
                    }
                    if let Statement::Assignment { id: id3, .. } = &inner_stmts[3] {
                        assert_eq!(id3.id, "x_1");
                    }
                    if let Statement::Declaration { id: id4, .. } = &inner_stmts[4] {
                        assert_eq!(id4.id, "z_0");
                    }
                }
            }
        }
    }
}
