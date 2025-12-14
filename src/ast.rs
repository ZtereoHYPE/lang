use std::collections::HashMap;

pub type SymbolTable = HashMap<Identifier, Symbol>;

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable {
        ty: Type
    },

    Function {
        ty: Type,
        params: Vec<Type>
    },

    Unknown // needs further analysis to be determined
}


#[derive(Debug, Clone)]
pub struct Program {
    pub symbols: SymbolTable, // for the functions
    pub items: Vec<Item>
}

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        id: Identifier,
        symbols: SymbolTable, // for the function parameters
        params: Vec<(Identifier, Type)>,
        ty: Type,
        body: Expression
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier {
    pub(crate) id: String
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I64,
    Bool,
    Unit,
}

#[derive(Debug, Clone)]
pub enum Expression {
    UnaryOp {
        op: Operator, 
        expr: Box<Expression>
    },
    BinaryOp {
        lhs: Box<Expression>,
        op: Operator,
        rhs: Box<Expression>
    },
    FunctionCall {
        id: Identifier,
        args: Vec<Expression>
    },
    Variable {
        id: Identifier
    },
    Literal(Literal),
    If {
        expression: Box<Expression>,
        then: Box<Expression>,
        else_expr: Option<Box<Expression>>
    },
    Block {
        symbols: SymbolTable,   // for the declarations
        statements: Vec<Statement>,
        expression: Option<Box<Expression>>
    }
}

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

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    I64(i64),
    Unit()
}

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



impl Literal {
    pub fn ty(&self) -> Type {
        match self {
            Literal::Bool(_) => Type::Bool,
            Literal::I64(_) => Type::I64,
            Literal::Unit() => Type::Unit
        }
    }
}

impl Operator {
    pub fn arg_type(&self) -> Type { match self {
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

    pub fn ret_type(&self) -> Type { match self {
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
