use crate::passes::parse::parse_file;
use crate::passes::rco::remove_complex_operands;
use crate::passes::resolve_symbols::resolve_symbols;
use crate::passes::resolve_types::resolve_types;
use std::fs;

mod states;
mod passes;

fn main() {
    let input_file = fs::read_to_string("lang.lang").expect("cannot read file");

    let mut ast = parse_file(input_file);
    resolve_symbols(&mut ast);
    resolve_types(&mut ast);

    // basic lowering preparation to ensure operands are atomic
    remove_complex_operands(&mut ast);

    // println!("{:?}", ast);
}
