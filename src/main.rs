use crate::passes::explicate_control::explicate_control;
use crate::passes::parse::parse_file;
use crate::passes::rco::remove_complex_operands;
use crate::passes::resolve_symbols::resolve_symbols;
use crate::passes::resolve_types::resolve_types;
use crate::passes::shrink::shrink_program;
use crate::passes::uniquify::uniquify_program;
use std::fs;
use crate::passes::liveness_analysis::analyze_liveness;

mod states;
mod passes;

fn main() {
    let input_file = fs::read_to_string("lang.lang").expect("cannot read file");

    let mut ast = parse_file(input_file);
    resolve_symbols(&mut ast);
    resolve_types(&mut ast);
    shrink_program(&mut ast);
    uniquify_program(&mut ast);
    remove_complex_operands(&mut ast);

    let mut ir = explicate_control(ast);
    analyze_liveness(&mut ir);

    println!("{:?}", ir);
}
