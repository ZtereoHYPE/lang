use crate::states::ast::{Expression, Identifier, Item, Program, Statement, SymbolTable, Type};

/// Remove Complex Operands
///
/// a basic pass to rewrite expressions so that operands of
/// UnaryOp or BinaryOp become atomic
/// - We treat Expression::Variable and Expression::Literal as atomic
/// - When a unary/binary/call/if/while needs an atomic subexpression, we:
///   1) recursively rco the subexpression,
///   2) if it isn't atomic, bind it to a new temporary using a let-like
///      Statement::Declaration inside a surrounding Expression::Block
///
/// assumptions:
/// - Temp declarations use type Unit for now, because we don't have a type
///   annotation on Expression nodes. Once we have added typing to the IR, change
///   this to the known type
pub fn remove_complex_operands(program: &mut Program) {
    let mut tmp_gen = TempGen::default();

    for item in &mut program.items {
        match item {
            Item::Function { body, .. } => {
                let (stmts, expr) = rco_expr(body.clone(), &mut tmp_gen);
                *body = if stmts.is_empty() {
                    expr
                } else {
                    Expression::Block {
                        symbols: SymbolTable::new(),
                        statements: stmts,
                        expression: Some(Box::new(expr)),
                    }
                };
            }
        }
    }
}

#[derive(Default)]
struct TempGen {
    counter: usize,
}

impl TempGen {
    fn fresh(&mut self) -> Identifier {
        self.counter += 1;
        Identifier {
            id: format!("_tmp{}", self.counter),
        }
    }
}

fn is_atomic(expr: &Expression) -> bool {
    matches!(expr, Expression::Variable { .. } | Expression::Literal(_))
}

/// rco an expression
/// returns statements to execute before and rewritten expression
fn rco_expr(expr: Expression, tmp_gen: &mut TempGen) -> (Vec<Statement>, Expression) {
    match expr {
        Expression::Variable { .. } | Expression::Literal(_) => (vec![], expr),
        Expression::UnaryOp { op, expr } => {
            let (mut s1, e1) = rco_expr(*expr, tmp_gen);
            let (mut s2, a1) = ensure_atomic(e1, tmp_gen);
            s1.append(&mut s2);
            (
                s1,
                Expression::UnaryOp {
                    op,
                    expr: Box::new(a1),
                },
            )
        }
        Expression::BinaryOp { lhs, op, rhs } => {
            let (mut s1, e1) = rco_expr(*lhs, tmp_gen);
            let (mut s2, e2) = rco_expr(*rhs, tmp_gen);

            let (mut s3, a1) = ensure_atomic(e1, tmp_gen);
            let (mut s4, a2) = ensure_atomic(e2, tmp_gen);

            s1.append(&mut s2);
            s1.append(&mut s3);
            s1.append(&mut s4);
            (
                s1,
                Expression::BinaryOp {
                    lhs: Box::new(a1),
                    op,
                    rhs: Box::new(a2),
                },
            )
        }
        Expression::FunctionCall { id, args } => {
            let mut out_stmts = Vec::new();
            let mut out_args = Vec::with_capacity(args.len());

            for arg in args {
                let (mut s, e) = rco_expr(arg, tmp_gen);
                let (mut s2, a) = ensure_atomic(e, tmp_gen);
                out_stmts.append(&mut s);
                out_stmts.append(&mut s2);
                out_args.push(a);
            }

            (out_stmts, Expression::FunctionCall {id, args: out_args})
        }
        Expression::If {
            expression,
            then,
            else_expr,
        } => {
            let (mut s1, cond) = rco_expr(*expression, tmp_gen);
            let (mut s2, cond_a) = ensure_atomic(cond, tmp_gen);
            s1.append(&mut s2);

            let then_new = {
                let (stmts, expr) = rco_expr(*then, tmp_gen);
                if stmts.is_empty() {
                    expr
                } else {
                    Expression::Block {
                        symbols: SymbolTable::new(),
                        statements: stmts,
                        expression: Some(Box::new(expr)),
                    }
                }
            };

            let else_new = if let Some(e) = else_expr {
                let (stmts, expr) = rco_expr(*e, tmp_gen);
                Some(Box::new(if stmts.is_empty() {
                    expr
                } else {
                    Expression::Block {
                        symbols: SymbolTable::new(),
                        statements: stmts,
                        expression: Some(Box::new(expr)),
                    }
                }))
            } else {
                None
            };
            (
                s1,
                Expression::If {
                    expression: Box::new(cond_a),
                    then: Box::new(then_new),
                    else_expr: else_new,
                },
            )
        }
        Expression::While { expression, block } => {
            // complex condition into stmts before loop, and rco the loop body
            let (mut s1, cond) = rco_expr(*expression, tmp_gen);
            let (mut s2, cond_a) = ensure_atomic(cond, tmp_gen);
            s1.append(&mut s2);

            let block_new = {
                let (stmts, expr) = rco_expr(*block, tmp_gen);
                if stmts.is_empty() {
                    expr
                } else {
                    Expression::Block {
                        symbols: SymbolTable::new(),
                        statements: stmts,
                        expression: Some(Box::new(expr)),
                    }
                }
            };
            (
                s1,
                Expression::While {
                    expression: Box::new(cond_a),
                    block: Box::new(block_new),
                },
            )
        }
        Expression::Block {
            symbols,
            statements,
            expression,
        } => {
            let mut out_statements = Vec::new();

            for st in statements {
                match st {
                    Statement::Assignment { id, expression } => {
                        let (mut s, e) = rco_expr(expression, tmp_gen);
                        out_statements.append(&mut s);
                        out_statements.push(Statement::Assignment {id, expression: e});
                    }
                    Statement::Declaration { id, ty, expression } => {
                        let (mut s, e) = rco_expr(expression, tmp_gen);
                        out_statements.append(&mut s);
                        out_statements.push(Statement::Declaration {id, ty, expression: e});
                    }
                    Statement::Expression(e) => {
                        let (mut s, e2) = rco_expr(e, tmp_gen);
                        out_statements.append(&mut s);
                        out_statements.push(Statement::Expression(e2));
                    }
                }
            }

            let out_expr = if let Some(expr) = expression {
                let (mut s, e) = rco_expr(*expr, tmp_gen);
                out_statements.append(&mut s);
                Some(Box::new(e))
            } else {
                None
            };

            (
                vec![],
                Expression::Block {
                    symbols,
                    statements: out_statements,
                    expression: out_expr,
                },
            )
        }
    }
}

fn ensure_atomic(expr: Expression, tmp_gen: &mut TempGen) -> (Vec<Statement>, Expression) {
    if is_atomic(&expr) {
        return (vec![], expr);
    }

    let tmp = tmp_gen.fresh();
    let decl = Statement::Declaration {
        id: tmp.clone(),
        // once expressions are typed, change to the correct type here
        ty: Type::Unit,
        expression: expr,
    };

    (
        vec![decl],
        Expression::Variable { id: tmp },
    )
}
