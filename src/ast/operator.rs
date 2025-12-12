use pest::iterators::Pair;
use crate::ast::{AstNode, AstParseError, Type};
use crate::Rule;

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Divided,
    Negated,
    And,
    Or,
    Xor,
    Not,
    GreaterThan,
    GreaterOrEqualThan,
    LessThan,
    LessOrEqualThan,
    Equal,
    Different
}

impl Operator {
    fn arg_type(&self) -> Type { match self {
        Operator::Plus |
        Operator::Minus |
        Operator::Times |
        Operator::Divided |
        Operator::Negated |
        Operator::GreaterThan |
        Operator::GreaterOrEqualThan |
        Operator::LessThan |
        Operator::LessOrEqualThan |
        Operator::Equal |
        Operator::Different => Type::I64,

        Operator::And |
        Operator::Or |
        Operator::Xor |
        Operator::Not => Type::Bool
    }}

    fn ret_type(&self) -> Type { match self {
        Operator::Plus |
        Operator::Minus |
        Operator::Times |
        Operator::Divided |
        Operator::Negated => Type::I64,

        Operator::And |
        Operator::Or |
        Operator::Not |
        Operator::Xor |
        Operator::GreaterThan |
        Operator::GreaterOrEqualThan |
        Operator::LessThan |
        Operator::LessOrEqualThan |
        Operator::Equal |
        Operator::Different => Type::Bool
    }}
}

impl AstNode for Operator {
    fn from_pair(op: Pair<Rule>) -> Result<Self, AstParseError> {
        let op = match op.as_rule() {
            Rule::addsub => if op.as_str() == "+"  {Self::Plus}     else {Self::Minus},
            Rule::logic  => if op.as_str() == "&&" {Self::And}      else {Self::Or}
            Rule::muldiv => if op.as_str() == "*"  {Self::Times}    else {Self::Divided},
            Rule::negnot => if op.as_str() == "-"  {Self::Negated}  else {Self::Not},
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
