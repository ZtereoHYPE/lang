use crate::interpreter::ast_interpreter::interpret_ast;
use crate::interpreter::ir_interpreter::IrInterpreter;
use crate::passes::explicate_control::explicate_control;
use crate::passes::liveness_analysis::analyze_liveness;
use crate::passes::parse::parse_file;
use crate::passes::rco::remove_complex_operands;
use crate::passes::resolve_symbols::resolve_symbols;
use crate::passes::resolve_types::resolve_types;
use crate::passes::shrink::shrink_program;
use crate::passes::uniquify::uniquify_program;
use crate::tui::run_tui;
use std::{env, fs};

mod passes;
mod representations;
mod tui;
mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    let test_mode = args.contains(&"-t".to_string());
    let path = args.iter().find(|a| a.ends_with(".lang")).expect("Please provide the path to a file to be compiled.");

    let input_file = fs::read_to_string(path).expect("Cannot read input file");

    if test_mode {
        run_test_mode(input_file);
    } else {
        run_tui_mode(input_file);
    }
}

fn run_test_mode(file: String) -> bool {
    let mut ast = parse_file(file);
    resolve_symbols(&mut ast);
    resolve_types(&mut ast);

    // Interpret the program before changes
    println!("running ast");
    let ast_value = interpret_ast(&ast);

    shrink_program(&mut ast);
    uniquify_program(&mut ast);
    remove_complex_operands(&mut ast);
    let mut ir = explicate_control(ast);
    analyze_liveness(&mut ir);

    println!("running ir");
    let ir_value = IrInterpreter::new(&ir).run();

    if ir_value == ast_value {
        println!("Both interpreters produed the same value! Result: {}", ir_value.to_string());
        true
    } else {
        println!("The interpreters produced differing values! ast: {}, ir: {}", ast_value.to_string(), ir_value.to_string());
        false
    }
}

fn run_tui_mode(file: String) {
    let mut ast = parse_file(file);
    resolve_symbols(&mut ast);
    resolve_types(&mut ast);
    shrink_program(&mut ast);
    uniquify_program(&mut ast);
    remove_complex_operands(&mut ast);
    let mut ir = explicate_control(ast);
    analyze_liveness(&mut ir);
    run_tui(ir).expect("Failed to render TUI!");
}


#[cfg(test)]
mod tests {
    use std::fs;
    use crate::run_test_mode;

    #[test]
    fn test_end_to_end_complex() {
        let input_file = fs::read_to_string("tests/complex.lang").expect("Cannot read test file");
        assert!(run_test_mode(input_file))
    }

    #[test]
    fn test_end_to_end_liveness() {
        let input_file = fs::read_to_string("tests/liveness.lang").expect("Cannot read test file");
        assert!(run_test_mode(input_file))
    }

    #[test]
    fn test_end_to_end_loops() {
        let input_file = fs::read_to_string("tests/loops.lang").expect("Cannot read test file");
        assert!(run_test_mode(input_file))
    }

    #[test]
    fn test_end_to_end_recursion() {
        let input_file = fs::read_to_string("tests/recursion.lang").expect("Cannot read test file");
        assert!(run_test_mode(input_file))
    }

}