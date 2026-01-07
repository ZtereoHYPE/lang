use crate::passes::parse::parse_file;
use crate::passes::resolve_symbols::resolve_symbols;
use crate::passes::resolve_types::resolve_types;
use crate::passes::shrink::shrink_program;
use std::fs;

mod states;
mod passes;

fn main() {
    let input_file = fs::read_to_string("lang.lang").expect("cannot read file");

    let mut ast = parse_file(input_file);

    resolve_symbols(&mut ast);
    resolve_types(&mut ast);
    shrink_program(&mut ast);

    println!("{:?}", ast);
}
