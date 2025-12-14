use crate::parse::parse_file;
use crate::resolve_symbols::resolve_symbols;
use std::fs;

mod ast;
mod resolve_symbols;
mod ir;
mod parse;
mod resolve_types;

fn main() {
    let input_file = fs::read_to_string("lang.lang").expect("cannot read file");

    let mut ast = parse_file(input_file);
    resolve_symbols(&mut ast);

    println!("{:?}", ast);
}
