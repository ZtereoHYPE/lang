use crate::representations::ast::Literal;

pub mod ast_interpreter;
pub mod ir_interpreter;

/// runtime value
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
}

impl From<&Literal> for Value {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Int(i) => Value::Int(*i),
            Literal::Unit() => Value::Unit,
        }
    }
}