use crate::states::ast::{Identifier, Literal, Operator};

pub struct Program {
    blocks: Vec<Block>,
}


pub struct Block {
    pub name: String,
    pub assignments: Vec<Assignment>,
    pub terminal: Box<Terminal>
}

pub struct Assignment {
    pub var: Identifier,
    pub expression: Expression
}

pub enum Expression {
    Unary {
        op: Operator,
        atom: Atom,
    },

    Binary {
        lhs: Atom,
        op: Operator,
        rhs: Atom
    }
}

pub enum Atom {
    Variable {
        id: Identifier
    },

    Value(Literal)
}

pub enum Terminal {
    Return {
        expression: Expression
    },

    Goto {
        label: String,
    },

    Conditional {
        condition: Expression,
        if_branch: Box<Terminal>,
        else_branch: Box<Terminal>
    },
}

// enum Terminal {
//     Return(Return),
//     Goto(Goto),
//     Conditional(Conditional),
// }
// struct Return {
//     expression: Expression
// }
// struct Goto {
//     label: String,
// }
// struct Conditional {
//     condition: Expression,
//     if_branch: Goto,
//     else_branch: Goto
// }
