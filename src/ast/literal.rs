use pest::iterators::Pair;
use crate::ast::{AstNode, AstParseError, Type};
use crate::Rule;

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    I64(i64),
    Unit()
}

impl Literal {
    fn ty(&self) -> Type {
        match self {
            Literal::Bool(_) => Type::Bool,
            Literal::I64(_) => Type::I64,
            Literal::Unit() => Type::Unit
        }
    }
}

impl AstNode for Literal {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        match pair.as_rule() {
            Rule::boolean => Ok(Literal::Bool(pair.as_str().parse().map_err(|_| AstParseError::new_str("Failed to parse boolean!"))?)),
            Rule::integer => Ok(Literal::I64(pair.as_str().parse().map_err(|_| AstParseError::new_str("Failed to parse integer!"))?)),
            Rule::unit    => Ok(Literal::Unit()),
            _ => Err(AstParseError::new_str("Bad literal type encountered."))
        }
    }
}

