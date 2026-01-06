use crate::naming::Namer;
use crate::states::{ast, ir};
use crate::states::ast::{Expression, Identifier};

pub fn explicate_control(program: ast::Program) -> ir::Program {
    let mut expl = ExplicateControl { namer: Namer::new(), blocks: vec![] };
    expl.explicate_program(program)
}

// state for ExplicateControl
struct ExplicateControl {
    namer: Namer,
    blocks: Vec<ir::Block>
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
        self.namer.set_fn_name(function.id.id.clone());

        let first_block = self.explicate_fn_body(function.body);
        self.blocks.insert(0, first_block);

        ir::Function {
            name: function.id,
            params: function.params.into_iter().map(|(i, _)| i).collect(),
            blocks: std::mem::take(&mut self.blocks), // this also clears self.blocks, ready for the next function. Neat!
        }
    }

    fn explicate_fn_body(&mut self, fn_body: ast::Expression) -> ir::Block {
        let name = self.namer.block_name("");
        match fn_body {
            // todo: waiting for 
            ast::Expression::If { expression, then, else_expr } => todo!("Waiting on previous pass"),
            ast::Expression::While { expression, block } => todo!("Waiting on previous pass"),
            ast::Expression::Block { statements, expression, .. } => todo!("Waiting on previous pass"),
            
            e => ir::Block {
                name,
                assignments: vec![],
                terminal: ir::Terminal::Return(self.explicate_expression(e)),
            }
        }
    }

    fn explicate_block(&mut self, expr: ast::Expression) -> ir::Block {match expr {
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

    fn explicate_statement(&mut self, statement: ast::Statement) -> (Identifier, ir::Expression) { match statement {
        ast::Statement::Assignment { id, expression } |
        ast::Statement::Declaration { id, expression, .. } => (id, self.explicate_expression(expression)),

        // Expression statements get assigned to temporary variables
        // because everything that isn't a terminal must be an assignment... uh
        ast::Statement::Expression(expr) => {
            let tmp_id = self.namer.get_unused_id();
            (tmp_id, self.explicate_expression(expr))
        }
    }}

    fn create_block(&mut self, block: ir::Block) -> String {
        // This block is just a goto to somewhere, return that somewhere directly.
        if let ir::Terminal::Goto{ label } = &block.terminal
            && block.assignments.is_empty() {
            return label.clone()
        }

        // Else, push the block to the new blocks, and return its label
        let label = block.name.clone();
        self.blocks.push(block);
        label
    }
}
