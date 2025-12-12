use pest::iterators::Pair;
use crate::ast::{check_pair, AstNode, AstParseError};
use crate::Rule;

#[derive(Debug, Clone)]
pub struct Identifier {
    id: String
}

impl AstNode for Identifier {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::id)?;

        Ok(Self { id: String::from(pair.as_str()) })
    }
}