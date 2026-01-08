use std::collections::HashMap;
use crate::states::ast::{Expression, Literal, Program, Operator, Statement, Identifier};

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
