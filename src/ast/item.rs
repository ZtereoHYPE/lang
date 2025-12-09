use pest::iterators::Pair;
use crate::ast::{check_pair, nth_inner, AstNode, AstParseError};
use crate::ast::expression::Expression;
use crate::ast::identifier::Identifier;
use crate::ast::r#type::Type;
use crate::Rule;

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        id: Identifier,
        params: Vec<(Identifier, Type)>,
        ty: Type,
        body: Expression
    }
}

impl AstNode for Item {
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

        Ok(Item::Function { id, params, ty, body })
    }
}
