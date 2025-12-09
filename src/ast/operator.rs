use pest::iterators::Pair;
use crate::ast::{AstNode, AstParseError};
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
    Not,
    GreaterThan,
    GreaterOrEqualThan,
    LessThan,
    LessOrEqualThan,
    Equal,
    Different
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
