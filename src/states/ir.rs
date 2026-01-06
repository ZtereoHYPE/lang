use std::collections::HashSet;
use crate::states::ast::{Identifier, Literal, Operator};

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<Function>,
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
    pub assignments: Vec<(Identifier, Expression)>,
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

impl Block {
    // for CFG navigation
    pub fn children(&self) -> HashSet<&String> { match &self.terminal {
        Terminal::Goto { label } => 
            HashSet::from([label]),
        
        Terminal::Conditional { then_label, else_label, .. } => 
            HashSet::from([then_label, else_label]),
        
        Terminal::Return(_) => HashSet::new()
    }}
}