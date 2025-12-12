use pest::iterators::Pair;
use crate::Rule;

pub mod statement;
pub mod expression;
pub mod operator;
pub mod identifier;
pub mod r#type;
pub mod program;
pub mod item;
pub mod literal;

pub use statement::*;
pub use expression::*;
pub use operator::*;
pub use identifier::*;
pub use r#type::*;
pub use program::*;
pub use item::*;

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

pub trait AstNode : Sized {
    fn from_pair(pair: Pair<Rule>) -> Result<Self, AstParseError>;
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