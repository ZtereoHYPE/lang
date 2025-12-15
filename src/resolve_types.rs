use crate::ast::Item::Function;
use crate::ast::{Expression, Item, Program, Statement, Symbol, Type};
use crate::resolve_symbols::ScopeStack;

struct TypeError {
    string: String
}

pub fn resolve_types(program: &mut Program) {
    let stack = ScopeStack {
        stack: vec![],
    };

    if let Err(e) = program.resolve_types(stack) {
        println!("Type error detected: {}", e.string)
    }
}

impl Program {
    fn resolve_types(&mut self, stack: ScopeStack) -> Result<(), TypeError> {
        // Add all the function types to the symbol table
        for Function {id, params, ty, ..} in &self.items {
            let params = params.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
            self.symbols.insert(id.clone(), Symbol::Function {ty: *ty, params});
        }

        // Recursively populate the symbols down the tree, with the additional local scope
        for item in &mut self.items {
            item.resolve_types(stack.with_scope(&self.symbols))?;
        }

        Ok(())
    }
}

impl Item {
    fn resolve_types(&mut self, stack: ScopeStack) -> Result<(), TypeError> { match self {
        Function { symbols, params, body, .. } => {
            // Add all parameters to the symbol list, as long as there's no collision
            for (id, ty) in params {
                symbols.insert(id.clone(), Symbol::Variable {ty: *ty});
            }

            body.resolve_type(stack.with_scope(symbols))?;
            Ok(())
        }
    }}
}

impl Expression {
    fn resolve_type(&mut self, stack: ScopeStack) -> Result<Type, TypeError> { match self {
        Expression::UnaryOp { op, expr } => {
            let expr_ty = expr.resolve_type(stack)?;
            if op.arg_type() != expr_ty {
                Err(TypeError {string: format!("Operator {:?} expected expression of type '{:?}' but received type '{:?}'.", op,op.arg_type(), expr_ty)})
            } else {
                Ok(op.ret_type())
            }
        },

        Expression::BinaryOp { lhs, op, rhs } => {
            let lhs_ty = lhs.resolve_type(stack.clone())?;
            let rhs_ty = rhs.resolve_type(stack)?;

            if lhs_ty != rhs_ty {
                Err(TypeError {string: format!("Operator {:?} expected lhs and rhs of same time but received '{:?}' and '{:?}'.", op, lhs_ty, rhs_ty)})

            } else if op.arg_type() != lhs_ty {
                Err(TypeError {string: format!("Operator {:?} expected expression of type '{:?}' but received type '{:?}'.", op,op.arg_type(), lhs_ty)})

            } else {
                Ok(op.ret_type())
            }
        }

        Expression::FunctionCall { id, args } => {
            // Check that the identifier points to a function and not a variable
            // todo: this code would be much simpler if we supported higher order functions!
            if let Some(Symbol::Function { ty, params }) = stack.resolve_symbol(id) {
                // Check for the right amount of arguments
                if params.len() != args.len() {
                    return Err(TypeError {string: format!("Wrong number of arguments in function '{}'", id.id)});
                }

                // check that the types of the arguments match the parameters
                for (arg, par) in args.iter_mut().zip(params) {
                    if arg.resolve_type(stack.clone())? != par {
                        return Err(TypeError {string: "Function argument of wrong type".to_string()});
                    }
                }

                Ok(ty)
            } else {
                Err(TypeError {string: format!("Failed to find function with name '{}'", id.id)})
            }
        },

        Expression::Variable { id } => {
            if let Some(Symbol::Variable {ty}) = stack.resolve_symbol(id) {
                Ok(ty)
            } else {
                Err(TypeError {string: format!("Failed to find variable with name '{}'", id.id)})
            }
        },

        Expression::Literal(l) => Ok(l.ty()),

        Expression::If { expression, then, else_expr } => {
            // Enforce boolean expression
            let expr_ty = expression.resolve_type(stack.clone())?;
            if expr_ty != Type::Bool {
                return Err(TypeError {string: format!("If statement expected boolean expression, but got '{:?}'", expr_ty)})
            }

            // todo: clean this logic up a little (use a match on the tuple!)
            let then_ty = then.resolve_type(stack.clone())?;
            if let Some(e) = else_expr {
                // If there's an else branch, it should match the type of then
                let else_ty = e.resolve_type(stack)?;
                if then_ty != else_ty {
                    Err(TypeError {string: format!("Two branches of if statement should return the same type, but got '{:?}' and '{:?}'", then_ty, else_ty)})
                } else {
                    Ok(then_ty)
                }
            } else {
                // Else, the then should return ()
                if then_ty != Type::Unit {
                    Err(TypeError {string: format!("Single branch if statement should return '()', but got '{:?}'", then_ty)})
                } else {
                    Ok(then_ty)
                }
            }
        },

        Expression::While { expression, block } => {
            // Enforce boolean expression
            let expr_ty = expression.resolve_type(stack.clone())?;
            if expr_ty != Type::Bool {
                return Err(TypeError {string: format!("While loop expected expression of type Bool, but got '{:?}'", expr_ty)})
            }

            // Enforce Unit body
            let block_ty = block.resolve_type(stack.clone())?;
            if block_ty != Type::Unit {
                return Err(TypeError {string: format!("While loop expected body of type Unit, but got '{:?}'", block_ty)})
            }

            Ok(Type::Unit)
        },

        Expression::Block { symbols, statements, expression } => {
            for statement in statements {
                match statement {
                    // Ensure that the variable being assigned is a variable, and of the right type.
                    // todo: would be much more elegant if we allowed higher order functions!
                    Statement::Assignment { id, expression } => {
                        let expr_ty = expression.resolve_type(stack.with_scope(symbols))?;

                        if let Some(Symbol::Variable {ty}) = stack.with_scope(symbols).resolve_symbol(id) {
                            if ty != expr_ty {
                                return Err(TypeError {string: format!("Assigned variable type doesn't match expression type. Expected '{:?}', got '{:?}'", ty, expr_ty)})
                            }
                        } else {
                            return Err(TypeError {string: format!("Expected variable for '{}', but found function", id.id)})
                        }
                    }

                    // Ensure that the expression's type matches the variable's type,
                    // and set the variable's type in the symbol table.
                    Statement::Declaration { id, expression, ty } => {
                        let expr_ty = expression.resolve_type(stack.with_scope(symbols))?;

                        if *ty != expr_ty {
                            return Err(TypeError {string: format!("Expected expression of type {:?}, but got {:?}", ty, expr_ty)})
                        }

                        symbols.insert(id.clone(), Symbol::Variable {ty: *ty});
                    }

                    // Resolve the expression with the extra variables created in the scope
                    Statement::Expression(expression) => {
                        expression.resolve_type(stack.with_scope(symbols))?;
                    }
                }
            }

            if let Some(expr) = expression {
                expr.resolve_type(stack.with_scope(symbols))
            } else {
                Ok(Type::Unit)
            }
        },
    }}
}
