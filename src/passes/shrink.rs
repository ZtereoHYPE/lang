use crate::states::ast::{Expression, Literal, Program, Operator, Statement};

// Shrinks a program by removing expressions not available in x86
pub fn shrink_program(program: &mut Program) {
    for function in &mut program.functions {
        function.body = shrink_expr(function.body.clone());
    }
}

pub fn shrink_expr(e: Expression) -> Expression {
    match e {
        Expression::BinaryOp { lhs, op, rhs } => {
            let l1 = Box::new(shrink_expr(*lhs));
            let r1 = Box::new(shrink_expr(*rhs));

            match op {
                Operator::And => {
                    Expression::If { 
                        expression: l1,
                        then: r1, 
                        else_expr: Some(Box::new(Expression::Literal(Literal::Bool(false))))
                    }
                }
                Operator::Or => {
                    Expression::If { 
                        expression: l1,
                        then: Box::new(Expression::Literal(Literal::Bool(true))),
                        else_expr: Some(r1)
                    }
                }
                Operator::Xor => {
                    Expression::If { 
                        expression: l1,
                        then: Box::new(Expression::UnaryOp { op: Operator::Not, expr: r1.clone() }),
                        else_expr: Some(r1)
                    }
                }
                _ => {
                    Expression::BinaryOp {
                        lhs: l1,
                        op: op.clone(),
                        rhs: r1,
                    }
                }
            }
        }

        Expression::Block { symbols, statements, expression } => {
            let statements = statements
                .into_iter()
                .map(|s| shrink_statement(s))
                .collect();

            let expression = expression
                .clone()
                .map(|e| Box::new(shrink_expr(*e)));

            Expression::Block {
                symbols: symbols.clone(),
                statements,
                expression
            }
        }

        Expression::FunctionCall { id, args } => {
            let mut new_args: Vec<Expression> = Vec::with_capacity(args.len());

            for arg in args {
                let expr = shrink_expr(arg);
                new_args.push(expr);
            }

            Expression::FunctionCall { id: id.clone(), args: new_args }
        }

        Expression::If { expression, then, else_expr } => {
            let n_cond = shrink_expr(*expression);
            let n_then = shrink_expr(*then);
            
            if else_expr.is_none() {
                Expression::If { 
                    expression: Box::new(n_cond), 
                    then: Box::new(n_then), 
                    else_expr: else_expr.clone()
                }
            }
            else {
                let n_else = shrink_expr(*else_expr.expect("Expression was None"));

                Expression::If { 
                    expression: Box::new(n_cond), 
                    then: Box::new(n_then), 
                    else_expr: Some(Box::new(n_else))
                }
            }
        }

        Expression::UnaryOp { op, expr } => {
            let n_expr = shrink_expr(*expr);

            Expression::UnaryOp { 
                op: op.clone(),
                expr: Box::new(n_expr) 
            }
        }


        Expression::While { expression, block } => {
            let n_cond = shrink_expr(*expression);
            let n_block = shrink_expr(*block);

            Expression::While {
                expression: Box::new(n_cond),
                block: Box::new(n_block)
            }
        }

        e => e.clone()
    }
}

fn shrink_statement(s: Statement) -> Statement { match s {
    Statement::Expression(e) =>
        Statement::Expression(shrink_expr(e)),

    Statement::Assignment { id, expression } =>
        Statement::Assignment { id, expression: shrink_expr(expression) },

    Statement::Declaration { id, ty, expression } =>
        Statement::Declaration { id, ty, expression: shrink_expr(expression) }
}}


#[cfg(test)]
mod tests {
    use crate::states::ast::Identifier;
    use crate::states::ast::Type;
    use super::*;

    #[test]
    fn test_shrink_and_operator() {
        let expr = Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Bool(true))),
            op: Operator::And,
            rhs: Box::new(Expression::Literal(Literal::Bool(false))),
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::If { .. });
    }

    #[test]
    fn test_shrink_or_operator() {
        let expr = Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Bool(false))),
            op: Operator::Or,
            rhs: Box::new(Expression::Literal(Literal::Bool(true))),
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::If { .. });
    }

    #[test]
    fn test_shrink_xor_operator() {
        let expr = Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Bool(true))),
            op: Operator::Xor,
            rhs: Box::new(Expression::Literal(Literal::Bool(true))),
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::If { .. });
    }

    #[test]
    fn test_shrink_arithmetic_operator() {
        let expr = Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(5))),
            op: Operator::Plus,
            rhs: Box::new(Expression::Literal(Literal::Int(3))),
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::BinaryOp { .. });
    }

    #[test]
    fn test_shrink_unary_op() {
        let expr = Expression::UnaryOp {
            op: Operator::Not,
            expr: Box::new(Expression::Literal(Literal::Bool(true))),
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::UnaryOp { .. });
    }

    #[test]
    fn test_shrink_if_with_else() {
        let expr = Expression::If {
            expression: Box::new(Expression::Literal(Literal::Bool(true))),
            then: Box::new(Expression::Literal(Literal::Int(1))),
            else_expr: Some(Box::new(Expression::Literal(Literal::Int(2)))),
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::If { .. });
    }

    #[test]
    fn test_shrink_if_without_else() {
        let expr = Expression::If {
            expression: Box::new(Expression::Literal(Literal::Bool(true))),
            then: Box::new(Expression::Literal(Literal::Int(1))),
            else_expr: None,
        };

        let result = shrink_expr(expr);
        matches!(result, Expression::If { .. });
    }

    #[test]
    fn test_shrink_literal() {
        let expr = Expression::Literal(Literal::Int(42));
        let result = shrink_expr(expr.clone());
        assert_eq!(format!("{:?}", result), format!("{:?}", expr));
    }

    #[test]
    fn test_shrink_assignment() {
        let stmt = Statement::Assignment {
            id: Identifier{id: "x".to_string()},
            expression: Expression::BinaryOp {
                lhs: Box::new(Expression::Literal(Literal::Int(1))),
                op: Operator::And,
                rhs: Box::new(Expression::Literal(Literal::Int(2))),
            },
        };

        let result = shrink_statement(stmt);
        matches!(result, Statement::Assignment { .. });
    }

    #[test]
    fn test_shrink_declaration() {
        let stmt = Statement::Declaration {
            id: Identifier{ id: "y".to_string() },
            ty: Type::Int,
            expression: Expression::Literal(Literal::Int(10)),
        };

        let result = shrink_statement(stmt);
        matches!(result, Statement::Declaration { .. });
    }
}
