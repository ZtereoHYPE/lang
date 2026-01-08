use std::collections::{HashMap, HashSet};
use crate::states::ast::{Identifier, Literal, Operator};

pub type Assignment = (Identifier, Expression);

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub entrypoint: String,
    pub blocks: HashMap<String, Block>
}

#[derive(Clone, Debug)]
pub struct Block {
    pub name: String,
    pub liveness: Vec<HashSet<Identifier>>,
    pub assignments: Vec<Assignment>,
    pub terminal: Terminal
}

#[derive(Clone, Debug)]
pub enum Terminal {
    Goto {
        label: String,
    },

    Conditional {
        condition: Atom,
        then_label: String,
        else_label: String
    },

    Return(Expression)
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
