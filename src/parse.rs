use crate::ast::{Expression, Identifier, Item, Literal, Operator, Program, Statement, Type};
use itertools::Itertools;
use pest::error::{ErrorVariant, LineColLocation};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, PrattParser, PrattParserMap};
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;
use std::process::exit;

#[derive(Parser)]
#[grammar = "lang.pest"]
struct LangParser;

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


#[derive(Debug)]
pub struct AstParseError {
    error: String
}

impl AstParseError {
    pub fn new_str(error: &'static str) -> Self {
        Self { error: String::from(error) }
    }
    pub fn new(error: String) -> Self {
        Self { error: error }
    }
}


pub fn parse_file(input_file: String) -> Program {
    let pairs = LangParser::parse(Rule::program, &input_file);

    if let Err(error) = &pairs {
        let (line_nr, col_nr) = match error.line_col {
            LineColLocation::Pos((l, c)) => (l, c),
            LineColLocation::Span((l, c), _) => (l, c),
        };

        let ErrorVariant::ParsingError{positives, negatives: _} = &error.variant
        else {unreachable!("we don't have custom errros!")};

        println!("Parsing error at {}:{}. ", line_nr, col_nr);
        println!("{}", error.line());
        println!("{}^", "-".repeat(col_nr - 1));
        println!("Was expecting {:?}!", positives);

        exit(-1)
    }

    let result = Program::from_pair(pairs.unwrap().next().unwrap());

    // todo: improve these compiler errors
    if let Err(e) = result {
        println!("Failed to parse! Error: {:?}", e);
        exit(-1)
    }

    result.unwrap()
}


pub trait FromParseTree : Sized {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError>;
}

impl FromParseTree for Program {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::program)?;
        let symbols = HashMap::new();

        Ok(Self {
            items: pair
                .into_inner()
                .map(|i| Item::from_pair(i))
                .process_results(|r| r.collect())?, // stop at the first Err
            symbols
        })
    }
}

impl FromParseTree for Item {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::function)?;

        let id =   Identifier::from_pair(nth_inner(&pair, 0))?;
        let ty =         Type::from_pair(nth_inner(&pair, 2))?;
        let body = Expression::from_pair(nth_inner(&pair, 3))?;

        let mut params = Vec::new();
        for param in nth_inner(&pair, 1).into_inner() {
            let mut inner = param.into_inner();
            let id = Identifier::from_pair(inner.next().unwrap())?;
            let ty =       Type::from_pair(inner.next().unwrap())?;
            params.push((id, ty))
        }

        let symbols = HashMap::new();

        Ok(Item::Function { id, params, ty, body, symbols })
    }
}

impl FromParseTree for Identifier {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::id)?;

        Ok(Self { id: String::from(pair.as_str()) })
    }
}

impl FromParseTree for Type {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::ty)?;
        let str = pair.as_str();

        Ok(if str == "I64" {Self::I64} else if str == "Bool" {Self::Bool} else {Self::Unit})
    }
}

impl FromParseTree for Expression {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        Self::get_pratt_parser().parse(Pairs::single(pair))
    }
}

impl Expression {
    fn get_pratt_parser<'pratt, 'a, 'i>() -> (PrattParserMap<'pratt, 'a, 'i, Rule, fn(Pair<'i, Rule>) -> Result<Expression, AstParseError>, Result<Expression, AstParseError>>) {
        PRATT_PARSER
            .map_primary(Self::map_primary as fn(_) -> _) // why cant it figure out it's a function on its own
            .map_infix(Self::map_infix)
            .map_prefix(Self::map_prefix)
    }

    fn from_pairs(pairs: Pairs<Rule>) -> Result<Self, AstParseError> {
        Self::get_pratt_parser().parse(pairs)
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

            let symbols = HashMap::new();

            Ok(Expression::Block { statements, expression, symbols })
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
}

impl FromParseTree for Operator {
    fn from_pair(op: Pair<Rule>) -> Result<Self, AstParseError> {
        let op = match op.as_rule() {
            Rule::addsub => if op.as_str() == "+"  {Self::Plus}     else {Self::Minus},
            Rule::muldiv => if op.as_str() == "*"  {Self::Times}    else {Self::Divided},
            Rule::negnot => if op.as_str() == "-"  {Self::Negated}  else {Self::Not},
            Rule::logic  => {
                let str = op.as_str();
                if      str == "&&" { Self::And }
                else if str == "|"  { Self::Or }
                else                { Self::Xor }
            }
            Rule::compar => {
                let str = op.as_str();
                if      str == ">"  {Self::GreaterThan}
                else if str == "<"  {Self::LessThan}
                else if str == ">=" {Self::GreaterOrEqualThan}
                else if str == "<=" {Self::LessOrEqualThan}
                else if str == "==" {Self::Equal}
                else                {Self::Different}
            }
            _ => return Err(AstParseError::new_str("Operator::parse() can only be called on operator nodes!")),
        };

        Ok(op)
    }
}

impl FromParseTree for Literal {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        match pair.as_rule() {
            Rule::boolean => Ok(Literal::Bool(pair.as_str().parse().map_err(|_| AstParseError::new_str("Failed to parse boolean!"))?)),
            Rule::integer => Ok(Literal::I64(pair.as_str().parse().map_err(|_| AstParseError::new_str("Failed to parse integer!"))?)),
            Rule::unit    => Ok(Literal::Unit()),
            _ => Err(AstParseError::new_str("Bad literal type encountered."))
        }
    }
}

impl FromParseTree for Statement {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let inner = nth_inner(&pair, 0);

        match inner.as_rule() {
            Rule::assignment => {
                let id =         Identifier::from_pair(nth_inner(&inner, 0))?;
                let expression = Expression::from_pair(nth_inner(&inner, 1))?;

                Ok(Statement::Assignment { id, expression })
            },

            Rule::declaration => {
                let id = Identifier::from_pair(nth_inner(&inner, 0))?;
                let ty = Type::from_pair(nth_inner(&inner, 1))?;
                let expression = Expression::from_pair(nth_inner(&inner, 2))?;

                Ok(Statement::Declaration { id, ty, expression })
            },

            Rule::expression => Ok(Statement::Expression(Expression::from_pair(inner)?)),

            _ => panic!("uh oh")
        }
    }
}

// some utility functions used throughout the ast stuff
fn nth_inner<'a>(pair: &Pair<'a, Rule>, n: usize) -> Pair<'a, Rule> {
    pair
        .clone()
        .into_inner()
        .skip(n)
        .next()
        .unwrap() // guaranteed by grammar, if used properly
}

fn nth_inner_opt<'a>(pair: &Pair<'a, Rule>, n: usize) -> Option<Pair<'a, Rule>> {
    pair
        .clone()
        .into_inner()
        .skip(n)
        .next()
}

fn check_pair(pair: Pair<Rule>, rule: Rule) -> Result<Pair<Rule>, AstParseError> {
    if pair.as_rule() != rule {
        return Err(AstParseError::new(format!("Expected rule {:?} but found {:?}.", rule, pair.as_rule())));
    }

    Ok(pair)
}