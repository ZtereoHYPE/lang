#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>
}

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        id: Identifier,
        params: Vec<(Identifier, Type)>,
        ty: Type,
        body: Expression
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier {
    pub(crate) id: String
}

#[derive(Debug, Clone)]
pub enum Type {
    I64,
    Bool,
    Unit,
}

#[derive(Debug, Clone)]
pub enum Expression {
    UnaryOp {
        op: Operator, // string or actual value here?
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
    fn ty(&self) -> Type {
        match self {
            Literal::Bool(_) => Type::Bool,
            Literal::I64(_) => Type::I64,
            Literal::Unit() => Type::Unit
        }
    }
}

impl Operator {
    fn arg_type(&self) -> Type { match self {
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

    fn ret_type(&self) -> Type { match self {
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
