use std::collections::HashMap;
use crate::states::{ast, ir};
use crate::states::ast::Identifier;

pub fn explicate_control(program: ast::Program) -> ir::Program {
    let mut expl = ExplicateControl { block_idx: 0, tmp_idx: 0, blocks: HashMap::new() };
    expl.explicate_program(program)
}

// state for ExplicateControl
struct ExplicateControl {
    block_idx: usize,
    tmp_idx: usize,
    blocks: HashMap<String, ir::Block>
}

impl ExplicateControl {
    fn explicate_program(&mut self, program: ast::Program) -> ir::Program {
        ir::Program {
            functions: program.functions
                .into_iter()
                .map(|i| self.explicate_function(i))
                .collect()
        }
    }

    fn explicate_function(&mut self, function: ast::Function) -> ir::Function {
        self.block_idx = 0;

        let first_label = self.explicate_fn_body(function.body);

        ir::Function {
            name: function.id,
            params: function.params.into_iter().map(|(i, _)| i).collect(),
            blocks: std::mem::take(&mut self.blocks), // this also clears self.blocks, ready for the next function. Neat!
        }
    }

    fn explicate_fn_body(&mut self, fn_body: ast::Expression) { // todo: string aint right
        let name = self.block_label();
        match fn_body {
            ast::Expression::If { expression, then, else_expr } => {
                let condition = self.explicate_atom(*expression); // assertion: if statements have atoms as expression
                // let after_label =
                // let then_label = self.explicate_block(*then); // maybe the name can be defined here..?
                // let else_label = if else_expr.is_some() {
                //     self.explicate_block(*else_expr.unwrap())
                // } else {
                //     after_label.clone() // if there is no else, jump directly to the after block
                // };
                //
                //
                // self.create_block(
                //     vec![],
                //     ir::Terminal::Return(self.explicate_expression(e))
                // )
            },
            ast::Expression::While { expression, block } => todo!("Waiting on previous pass"),
            ast::Expression::Block { statements, expression, .. } => todo!("Waiting on previous pass"),

            e => {self.create_block(
                vec![],
                ir::Terminal::Return(self.explicate_expression(e))
            );}
        }
    }

    fn explicate_block(&mut self, expr: ast::Expression) -> Vec<ir::Assignment> { match expr {
        _ => todo!("Waiting on previous pass")
    }}

    fn explicate_expression(&mut self, expr: ast::Expression) -> ir::Expression { match expr {
        ast::Expression::UnaryOp { op, expr } => ir::Expression::Unary {
            op,
            atom: self.explicate_atom(*expr)
        },

        ast::Expression::BinaryOp { lhs, op, rhs } => ir::Expression::Binary {
            lhs: self.explicate_atom(*lhs),
            op,
            rhs: self.explicate_atom(*rhs)
        },

        ast::Expression::FunctionCall { id, args } => {
            let args = args.into_iter().map(|a| self.explicate_atom(a)).collect();
            ir::Expression::FunCall { id, args }
        }

        atom => ir::Expression::Atom(self.explicate_atom(atom)),
    }}

    fn explicate_atom(&mut self, atom: ast::Expression) -> ir::Atom { match atom {
        ast::Expression::Variable { id } => ir::Atom::Variable { id },
        ast::Expression::Literal(val) => ir::Atom::Value(val),

        e => panic!("Expression should be an Atom! Found {:?} instead", e)
    }}

    // todo: these are wrong! Expressions here don't have to be the limited IR expression, they can be if or blocks!
    fn explicate_statement(&mut self, statement: ast::Statement) -> ir::Assignment { match statement {
        ast::Statement::Assignment { id, expression } |
        ast::Statement::Declaration { id, expression, .. } => (id, self.explicate_expression(expression)),

        // Expression statements get assigned to temporary variables
        // because everything that isn't a terminal must be an assignment... uh
        ast::Statement::Expression(expr) => {
            let tmp_id = self.namer.get_unused_id();
            (tmp_id, self.explicate_expression(expr))
        }
    }}

    fn create_block(&mut self, assignments: Vec<(Identifier, ir::Expression)>, terminal: ir::Terminal) {
        // This block is just a goto to somewhere, return that somewhere directly.
        if let ir::Terminal::Goto{ label } = &terminal && assignments.is_empty() {
            return;
        }

        // Else, push the block to the new blocks, and return its label
        let name = self.block_label();
        self.blocks.push(ir::Block { name, assignments, terminal });
    }

    pub fn block_label(&mut self) -> String {
        self.block_idx += 1;
        format!("block_{}", self.block_idx)
    }

    pub fn tmp_id(&mut self) -> Identifier {
        self.tmp_idx += 1;
        Identifier { id: format!("_{}", self.tmp_idx) }
    }
}
