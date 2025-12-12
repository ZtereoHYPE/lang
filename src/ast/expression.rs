use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, PrattParser, PrattParserMap};
use crate::ast::{nth_inner, nth_inner_opt, AstNode, AstParseError};
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::operator::Operator;
use crate::ast::r#type::Type;
use crate::ast::statement::Statement;
use crate::Rule;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::Op;

        PrattParser::new()
            .op(Op::infix(Rule::addsub, Assoc::Left))
            .op(Op::infix(Rule::logic, Assoc::Left))
            .op(Op::infix(Rule::compar, Assoc::Left))
            .op(Op::infix(Rule::muldiv, Assoc::Left))
            .op(Op::prefix(Rule::negnot))
    };
}

#[derive(Debug, Clone)]
pub enum Expression {
    UnaryOp {
        op: Operator, // string or actual value here?
        expr: Box<Expression>
    },
    BinaryOp {
        lhs: Box<Expression>,
        op: Operator,
        rhs: Box<Expression>
    },
    FunctionCall {
        id: Identifier,
        args: Vec<Expression>
    },
    Variable {
        id: Identifier
    },
    Literal(Literal),
    If {
        expression: Box<Expression>,
        then: Box<Expression>,
        else_expr: Option<Box<Expression>>
    },
    Block {
        statements: Vec<Statement>,
        expression: Option<Box<Expression>>
    }
}

impl AstNode for Expression {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        Self::get_pratt_parser().parse(Pairs::single(pair))
    }
}

impl Expression {
    fn get_pratt_parser<'pratt, 'a, 'i>() -> (PrattParserMap<'pratt, 'a, 'i, Rule, fn(Pair<'i, Rule>) -> Result<Expression, AstParseError>, Result<Expression, AstParseError>>) {
        PRATT_PARSER
            .map_primary(map_primary as fn(_) -> _) // why cant it figure out it's a function on its own
            .map_infix(map_infix)
            .map_prefix(map_prefix)
    }

    fn from_pairs(pairs: Pairs<Rule>) -> Result<Self, AstParseError> {
        Self::get_pratt_parser().parse(pairs)
    }
}

fn map_primary(primary: Pair<Rule>) -> Result<Expression, AstParseError> {match primary.as_rule() {
    Rule::literal    => Ok(Expression::Literal(Literal::from_pair(primary)?)),
    Rule::id         => Ok(Expression::Variable { id: Identifier::from_pair(primary)? }),
    Rule::expression => Expression::from_pairs(primary.into_inner()),

    Rule::block => {
        let mut statements = Vec::new();
        let mut expression = None;

        for inner in primary.into_inner() {
            match inner.as_rule() {
                Rule::statement => statements.push(Statement::from_pair(inner)?),
                Rule::expression => expression = Option::Some(Box::new(Expression::from_pair(inner)?)),
                _ => unreachable!("Grammar guarantees that blocks only contain statements or expressions.")
            }
        }

        Ok(Expression::Block { statements, expression })
    },

    Rule::ifElse => {
        let expression = Box::new(Expression::from_pair(nth_inner(&primary, 0))?);
        let then =       Box::new(Expression::from_pair(nth_inner(&primary, 1))?);

        let else_expr =
            if let Some(e) = nth_inner_opt(&primary, 2) {
                Some(Box::new(Expression::from_pair(e)?))
            } else {
                None
            };

        Ok(Expression::If { expression, then, else_expr })
    },

    Rule::call => {
        let id = Identifier::from_pair(nth_inner(&primary, 0))?;
        let mut args = Vec::new();

        for inner in primary.into_inner().skip(1) {
            match inner.as_rule() {
                Rule::expression => args.push(Expression::from_pair(inner)?),
                _ => unreachable!("Grammar guarantees that calls args only contain expressions.")
            }
        }

        Ok(Expression::FunctionCall { id, args })
    },

    _ => panic!("This should never be reachable from within an expression: {:?}", primary.as_rule()),
}}

fn map_infix(lhs: Result<Expression, AstParseError>, op: Pair<Rule>, rhs: Result<Expression, AstParseError>) -> Result<Expression, AstParseError> {
    Ok(Expression::BinaryOp {
        lhs: Box::new(lhs?),
        op: Operator::from_pair(op)?,
        rhs: Box::new(rhs?)
    })
}

fn map_prefix(op: Pair<Rule>, expr: Result<Expression, AstParseError>) -> Result<Expression, AstParseError> {
    Ok(Expression::UnaryOp { op: Operator::from_pair(op)?, expr: Box::new(expr?) })
}

