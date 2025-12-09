use std::fs;

use pest::{Parser, error::{ErrorVariant, LineColLocation}, iterators::{Pair, Pairs}};
use pest_derive::Parser;
use crate::ast::AstNode;

mod ast;

#[derive(Parser)]
#[grammar = "lang.pest"]
struct LangParser;

fn main() {
    let input_file = fs::read_to_string("lang.lang").expect("cannot read file");

    let pairs = LangParser::parse(Rule::program, &input_file);

    if let Err(error) = &pairs {
        let (line_nr, col_nr) = match error.line_col {
            LineColLocation::Pos((l, c)) => (l, c),
            LineColLocation::Span((l, c), _) => (l, c),
        };

        let ErrorVariant::ParsingError{positives, negatives: _} = &error.variant
        else {unreachable!("we don't have custom errros!")};

        println!("Parsing error at {}:{}. ", line_nr, col_nr);
        println!("{}", error.line());
        println!("{}^", "-".repeat(col_nr - 1));
        println!("Was expecting {:?}!", positives);

        return;
    }

    let ast = ast::Program::from_pair(pairs.unwrap().next().unwrap());
    println!("{:?}", ast);
}
