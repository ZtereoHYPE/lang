use crate::states::ast::{Identifier, Literal, Operator};

pub struct Program {
    pub blocks: Vec<Function>,
}

pub struct Function {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub blocks: Vec<Block>
}

pub struct Block {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

pub enum Instruction {
    Assignment {
        var: Identifier,
        expr: Expression,
    },

    Goto {
        label: String,
    },

    Conditional {
        condition: Atom,
        then_label: String,
        else_label: String
    },
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
    },

    FunCall {
        id: Identifier,
        args: Vec<Atom>
    },

    Atom(Atom)
}

pub enum Atom {
    Variable {
        id: Identifier
    },

    Value(Literal)
}

impl Block {
    pub fn from_label(name: String) -> Self {
        Self { name, instructions: vec![] }
    }
}