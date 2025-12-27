use crate::states::ast::{Identifier, Literal, Operator};

#[derive(Clone, Debug)]
pub struct Program {
    pub blocks: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub blocks: Vec<Block>
}

#[derive(Clone, Debug)]
pub struct Block {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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