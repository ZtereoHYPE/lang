use itertools::Itertools;
use pest::iterators::Pair;
use crate::ast::{check_pair, AstNode, AstParseError};
use crate::ast::item::Item;
use crate::Rule;

#[derive(Debug, Clone)]
pub struct Program {
    items: Vec<Item>
}

impl AstNode for Program {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError> {
        let pair = check_pair(pair, Rule::program)?;

        Ok(Self {
            items: pair
                .into_inner()
                .map(|i| Item::from_pair(i))
                .process_results(|r| r.collect())? // stop at the first Err
        })
    }
}
