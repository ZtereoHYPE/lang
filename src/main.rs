use std::fs;
use crate::parser::parse_file;

mod ast;
mod semantics;
mod ir;
mod parser;

fn main() {
    let input_file = fs::read_to_string("lang.lang").expect("cannot read file");

    let ast = parse_file(input_file);

    println!("{:?}", ast);
}
