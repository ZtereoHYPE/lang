use pest::iterators::Pair;
use crate::ast::{check_pair, AstNode, AstParseError};
use crate::Rule;

#[derive(Debug, Clone)]
pub enum Type {
    I64,
    Bool,
    Unit,
}

impl AstNode for Type {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::ty)?;
        let str = pair.as_str();

        Ok(if str == "I64" {Self::I64} else if str == "Bool" {Self::Bool} else {Self::Unit})
    }
}
