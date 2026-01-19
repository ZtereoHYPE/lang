use crate::representations::ast::{Expression, Identifier, Literal, Program, Statement, SymbolTable, Type};

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

    for function in &mut program.functions {
        let (stmts, expr) = rco_expr(function.body.clone(), &mut tmp_gen);
        function.body = if stmts.is_empty() {
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

            (out_stmts, Expression::FunctionCall { id, args: out_args })
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
            // rco the condition
            let (mut header_stmts, cond_expr) = rco_expr(*expression, tmp_gen);
            let (mut decl_stmts, cond_atomic) = ensure_atomic(cond_expr, tmp_gen);
            header_stmts.append(&mut decl_stmts);

            // rco the loop body
            let (body_stmts, body_expr) = rco_expr(*block, tmp_gen);
            let body = if body_stmts.is_empty() {
                body_expr
            } else {
                Expression::Block {
                    symbols: SymbolTable::new(),
                    statements: body_stmts,
                    expression: Some(Box::new(body_expr)),
                }
            };

            // wrap condition statements in a block so they run each iteration
            let header = if header_stmts.is_empty() {
                cond_atomic
            } else {
                Expression::Block {
                    symbols: SymbolTable::new(),
                    statements: header_stmts,
                    expression: Some(Box::new(cond_atomic)),
                }
            };

            (
                vec![],
                Expression::While {
                    expression: Box::new(header),
                    block: Box::new(body),
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
                        out_statements.push(Statement::Assignment { id, expression: e });
                    }
                    Statement::Declaration { id, ty, expression } => {
                        let (mut s, e) = rco_expr(expression, tmp_gen);
                        out_statements.append(&mut s);
                        out_statements.push(Statement::Declaration {
                            id,
                            ty,
                            expression: e,
                        });
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
                Some(Box::new(Expression::Literal(Literal::Unit())))
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

    (vec![decl], Expression::Variable { id: tmp })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::representations::ast::{Function, Operator};

    // ========== Helper functions ==========

    fn id(name: &str) -> Identifier {
        Identifier { id: name.to_string() }
    }

    fn var(name: &str) -> Expression {
        Expression::Variable { id: id(name) }
    }

    fn int(n: i64) -> Expression {
        Expression::Literal(Literal::Int(n))
    }

    fn binary(lhs: Expression, op: Operator, rhs: Expression) -> Expression {
        Expression::BinaryOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    fn unary(op: Operator, expr: Expression) -> Expression {
        Expression::UnaryOp {
            op,
            expr: Box::new(expr),
        }
    }

    fn call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::FunctionCall {
            id: id(name),
            args,
        }
    }

    fn if_expr(cond: Expression, then: Expression, else_e: Option<Expression>) -> Expression {
        Expression::If {
            expression: Box::new(cond),
            then: Box::new(then),
            else_expr: else_e.map(|e| Box::new(e)),
        }
    }

    fn while_expr(cond: Expression, body: Expression) -> Expression {
        Expression::While {
            expression: Box::new(cond),
            block: Box::new(body),
        }
    }

    fn block(stmts: Vec<Statement>, expr: Option<Expression>) -> Expression {
        Expression::Block {
            symbols: SymbolTable::new(),
            statements: stmts,
            expression: expr.map(|e| Box::new(e)),
        }
    }

    fn decl(name: &str, expr: Expression) -> Statement {
        Statement::Declaration {
            id: id(name),
            ty: Type::Int,
            expression: expr,
        }
    }

    fn assign(name: &str, expr: Expression) -> Statement {
        Statement::Assignment {
            id: id(name),
            expression: expr,
        }
    }

    fn expr_stmt(expr: Expression) -> Statement {
        Statement::Expression(expr)
    }

    fn make_program(body: Expression) -> Program {
        Program {
            symbols: SymbolTable::new(),
            functions: vec![Function {
                id: id("main"),
                symbols: SymbolTable::new(),
                params: vec![],
                ty: Type::Int,
                body,
            }],
        }
    }

    /// Check that an expression is atomic (variable or literal)
    fn assert_atomic(expr: &Expression) {
        assert!(is_atomic(expr), "Expected atomic expression, got {:?}", expr);
    }

    /// Recursively check that all operands of binary/unary ops are atomic
    fn assert_all_operands_atomic(expr: &Expression) {
        match expr {
            Expression::Variable { .. } | Expression::Literal(_) => {}
            Expression::UnaryOp { expr, .. } => {
                assert_atomic(expr);
                assert_all_operands_atomic(expr);
            }
            Expression::BinaryOp { lhs, rhs, .. } => {
                assert_atomic(lhs);
                assert_atomic(rhs);
                assert_all_operands_atomic(lhs);
                assert_all_operands_atomic(rhs);
            }
            Expression::FunctionCall { args, .. } => {
                for arg in args {
                    assert_atomic(arg);
                }
            }
            Expression::If { expression, then, else_expr } => {
                assert_atomic(expression);
                assert_all_operands_atomic(then);
                if let Some(e) = else_expr {
                    assert_all_operands_atomic(e);
                }
            }
            Expression::While { expression, block } => {
                // Condition might be a block containing the atomic condition
                assert_all_operands_atomic(expression);
                assert_all_operands_atomic(block);
            }
            Expression::Block { statements, expression, .. } => {
                for stmt in statements {
                    match stmt {
                        Statement::Declaration { expression, .. } |
                        Statement::Assignment { expression, .. } => {
                            assert_all_operands_atomic(expression);
                        }
                        Statement::Expression(e) => {
                            assert_all_operands_atomic(e);
                        }
                    }
                }
                if let Some(e) = expression {
                    assert_all_operands_atomic(e);
                }
            }
        }
    }

    /// Count how many temp variables (_tmpN) are declared
    fn count_temps(expr: &Expression) -> usize {
        match expr {
            Expression::Variable { .. } | Expression::Literal(_) => 0,
            Expression::UnaryOp { expr, .. } => count_temps(expr),
            Expression::BinaryOp { lhs, rhs, .. } => count_temps(lhs) + count_temps(rhs),
            Expression::FunctionCall { args, .. } => args.iter().map(count_temps).sum(),
            Expression::If { expression, then, else_expr } => {
                count_temps(expression) + count_temps(then) 
                    + else_expr.as_ref().map_or(0, |e| count_temps(e))
            }
            Expression::While { expression, block } => {
                count_temps(expression) + count_temps(block)
            }
            Expression::Block { statements, expression, .. } => {
                let stmt_temps: usize = statements.iter().map(|s| match s {
                    Statement::Declaration { id, expression, .. } => {
                        let is_tmp = id.id.starts_with("_tmp");
                        count_temps(expression) + if is_tmp { 1 } else { 0 }
                    }
                    Statement::Assignment { expression, .. } => count_temps(expression),
                    Statement::Expression(e) => count_temps(e),
                }).sum();
                stmt_temps + expression.as_ref().map_or(0, |e| count_temps(e))
            }
        }
    }

    // Tests

    #[test]
    fn test_atomic_unchanged() {
        let mut prog = make_program(var("x"));
        remove_complex_operands(&mut prog);
        match &prog.functions[0].body {
            Expression::Variable { id } => assert_eq!(id.id, "x"),
            _ => panic!("expected variable to remain unchanged"),
        }
    }

    #[test]
    fn test_literal_unchanged() {
        let mut prog = make_program(int(42));
        remove_complex_operands(&mut prog);
        match &prog.functions[0].body {
            Expression::Literal(Literal::Int(42)) => {}
            _ => panic!("expected literal to remain unchanged"),
        }
    }

    #[test]
    fn test_binary_atomic_no_temps() {
        let mut prog = make_program(binary(var("x"), Operator::Plus, var("y")));
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 0);
    }

    #[test]
    fn test_binary_complex_creates_temps() {
        let lhs = binary(var("a"), Operator::Plus, var("b"));
        let rhs = binary(var("c"), Operator::Plus, var("d"));
        let expr = binary(lhs, Operator::Plus, rhs);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);    
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 2);
    }

    #[test]
    fn test_unary_on_binary() {
        let inner = binary(var("a"), Operator::Plus, var("b"));
        let expr = unary(Operator::Negated, inner);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 1);
    }

    #[test]
    fn test_mixed_atomic_complex() {
        let rhs = binary(var("y"), Operator::Times, var("z"));
        let expr = binary(var("x"), Operator::Plus, rhs);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 1);
    }

    #[test]
    fn test_function_call_complex_args() {
        let arg1 = binary(var("a"), Operator::Plus, var("b"));
        let arg2 = binary(var("c"), Operator::Times, var("d"));
        let expr = call("f", vec![arg1, arg2]);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 2);
    }

    #[test]
    fn test_nested_function_calls() {
        let inner = call("g", vec![var("x")]);
        let expr = call("f", vec![inner]);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 1);
    }

    #[test]
    fn test_if_complex_condition() {
        let cond = binary(var("a"), Operator::GreaterThan, var("b"));
        let expr = if_expr(cond, int(1), Some(int(2)));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 1);
    }

    #[test]
    fn test_if_branches_processed() {
        let cond = var("c");
        let then_branch = binary(var("a"), Operator::Plus, var("b"));
        let else_branch = binary(var("x"), Operator::Times, var("y"));
        let expr = if_expr(cond, then_branch, Some(else_branch));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
    }

    #[test]
    fn test_while_condition_not_hoisted() {
        let cond = binary(var("a"), Operator::LessThan, var("b"));
        let body = var("x");
        let expr = while_expr(cond, body);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        match &prog.functions[0].body {
            Expression::While { expression, .. } => {
                match expression.as_ref() {
                    Expression::Block { statements, expression: inner_expr, .. } => {
                        assert!(!statements.is_empty(), "condition bindings should be inside the while condition");
                        assert!(inner_expr.is_some());
                        assert_atomic(inner_expr.as_ref().unwrap());
                    }
                    _ => panic!("complex while condition should be wrapped in a block"),
                }
            }
            _ => panic!("expected while expression"),
        }
    }

    #[test]
    fn test_block_statements_processed() {
        let stmt = decl("y", binary(var("a"), Operator::Plus, var("b")));
        let expr = block(vec![stmt], Some(var("y")));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
    }

    #[test]
    fn test_assignment_processed() {
        let stmt = assign("x", binary(var("a"), Operator::Times, var("b")));
        let expr = block(vec![stmt], Some(var("x")));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
    }

    #[test]
    fn test_expression_statement_processed() {
        let stmt = expr_stmt(call("print", vec![binary(var("a"), Operator::Plus, var("b"))]));
        let expr = block(vec![stmt], Some(int(0)));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
    }

    #[test]
    fn test_triple_nested() {
        let ab = binary(var("a"), Operator::Plus, var("b"));
        let cd = binary(var("c"), Operator::Plus, var("d"));
        let ef = binary(var("e"), Operator::Plus, var("f"));
        let ab_cd = binary(ab, Operator::Times, cd);
        let full = binary(ab_cd, Operator::Minus, ef);
        let mut prog = make_program(full);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 4);
    }

    #[test]
    fn test_evaluation_order() {
        let lhs = binary(var("a"), Operator::Plus, var("b"));
        let rhs = binary(var("c"), Operator::Plus, var("d"));
        let expr = binary(lhs, Operator::Plus, rhs);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        
        match &prog.functions[0].body {
            Expression::Block { statements, .. } => {
                assert!(statements.len() >= 2);
                
                if let Statement::Declaration { id, expression, .. } = &statements[0] {
                    assert_eq!(id.id, "_tmp1");
                    match expression {
                        Expression::BinaryOp { lhs, rhs, .. } => {
                            match (lhs.as_ref(), rhs.as_ref()) {
                                (Expression::Variable { id: l }, Expression::Variable { id: r }) => {
                                    assert_eq!(l.id, "a");
                                    assert_eq!(r.id, "b");
                                }
                                _ => panic!("expected a + b"),
                            }
                        }
                        _ => panic!("expected binary op"),
                    }
                }
                
                if let Statement::Declaration { id, expression, .. } = &statements[1] {
                    assert_eq!(id.id, "_tmp2");
                    match expression {
                        Expression::BinaryOp { lhs, rhs, .. } => {
                            match (lhs.as_ref(), rhs.as_ref()) {
                                (Expression::Variable { id: l }, Expression::Variable { id: r }) => {
                                    assert_eq!(l.id, "c");
                                    assert_eq!(r.id, "d");
                                }
                                _ => panic!("expected c + d"),
                            }
                        }
                        _ => panic!("expected binary op"),
                    }
                }
            }
            _ => panic!("expected block"),
        }
    }

    #[test]
    fn test_empty_block_gets_unit() {
        let expr = block(vec![], None);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        match &prog.functions[0].body {
            Expression::Block { expression, .. } => {
                match expression.as_ref().map(|e| e.as_ref()) {
                    Some(Expression::Literal(Literal::Unit())) => {}
                    other => panic!("expected Unit literal, got {:?}", other),
                }
            }
            _ => panic!("expected block"),
        }
    }

    #[test]
    fn test_unary_atomic_no_temp() {
        let expr = unary(Operator::Negated, var("x"));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        match &prog.functions[0].body {
            Expression::UnaryOp { expr, .. } => {
                assert_atomic(expr);
            }
            _ => panic!("expected unary op"),
        }
        assert_eq!(count_temps(&prog.functions[0].body), 0);
    }

    #[test]
    fn test_not_complex() {
        let inner = binary(var("a"), Operator::And, var("b"));
        let expr = unary(Operator::Not, inner);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 1);
    }

    #[test]
    fn test_while_body_processed() {
        let cond = var("c");
        let body = block(
            vec![assign("x", binary(var("x"), Operator::Plus, int(1)))],
            Some(var("x"))
        );
        let expr = while_expr(cond, body);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
    }

    #[test]
    fn test_multiple_functions() {
        let mut prog = Program {
            symbols: SymbolTable::new(),
            functions: vec![
                Function {
                    id: id("tmp1"),
                    symbols: SymbolTable::new(),
                    params: vec![],
                    ty: Type::Int,
                    body: binary(var("a"), Operator::Plus, var("b")),
                },
                Function {
                    id: id("tmp2"),
                    symbols: SymbolTable::new(),
                    params: vec![],
                    ty: Type::Int,
                    body: binary(
                        binary(var("x"), Operator::Times, var("y")),
                        Operator::Plus,
                        var("z")
                    ),
                },
            ],
        };
        remove_complex_operands(&mut prog);
        assert_eq!(count_temps(&prog.functions[0].body), 0);
        assert_eq!(count_temps(&prog.functions[1].body), 1);
        assert_all_operands_atomic(&prog.functions[1].body);
    }

    #[test]
    fn test_nullary_function_call() {
        let expr = binary(call("f", vec![]), Operator::Plus, int(1));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert_eq!(count_temps(&prog.functions[0].body), 1);
    }

    #[test]
    fn test_if_no_else() {
        let cond = binary(var("a"), Operator::Equal, int(0));
        let then_branch = call("print", vec![var("a")]);
        let expr = if_expr(cond, then_branch, None);
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
    }

    #[test]
    fn test_block_as_operand() {
        let blk = block(vec![decl("t", int(1))], Some(var("t")));
        let expr = binary(blk, Operator::Plus, int(2));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog);
        assert_all_operands_atomic(&prog.functions[0].body);
        assert!(count_temps(&prog.functions[0].body) >= 1);
    }

    #[test]
    fn test_if_as_operand() {
        let if_e = if_expr(var("c"), int(1), Some(int(2)));
        let expr = binary(if_e, Operator::Times, int(3));
        let mut prog = make_program(expr);
        remove_complex_operands(&mut prog); 
        assert_all_operands_atomic(&prog.functions[0].body);
        assert!(count_temps(&prog.functions[0].body) >= 1);
    }
}
