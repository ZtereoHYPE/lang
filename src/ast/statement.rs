use pest::iterators::Pair;
use crate::ast::{nth_inner, AstNode, AstParseError, Expression, Identifier, Type};
use crate::Rule;

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment {
        id: Identifier,
        expression: Expression,
    },
    Declaration {
        id: Identifier,
        ty: Type,
        expression: Expression
    },
    Expression(Expression)
}


impl AstNode for Statement {
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