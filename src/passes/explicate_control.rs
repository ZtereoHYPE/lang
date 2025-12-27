use crate::naming::Namer;
use crate::states::{ast, ir};

pub fn explicate_control(program: ast::Program) -> ir::Program {
    let mut namer = Namer::new();
    program.explicate_control(&mut namer)
}

impl ast::Program {
    fn explicate_control(self, namer: &mut Namer) -> ir::Program {
        ir::Program {
            blocks: self.functions
                .into_iter()
                .map(|i| i.explicate_control(namer))
                .collect()
        }
    }
}

impl ast::Function {
    fn explicate_control(self, namer: &mut Namer) -> ir::Function {
        let mut blocks = vec![ir::Block::from_label(format!("{}_0", self.id.id))];
        let mut new_blocks = self.body.explicate_control(namer, blocks.last_mut().unwrap());
        blocks.append(&mut new_blocks);

        ir::Function {
            name: self.id,
            params: self.params.into_iter().map(|(i, _)| i).collect(),
            blocks,
        }
    }
}

impl ast::Expression {
    fn explicate_control(self, namer: &mut Namer, last_block: &mut ir::Block) -> Vec<ir::Block> { match self {
        ast::Expression::If { expression, then, else_expr } => {
            // Create required labels
            let then_label = namer.block_name(&last_block, "if");
            let after_label = namer.block_name(&last_block, "");
            let else_label = if else_expr.is_some() {
                namer.block_name(&last_block, "else")
            } else {
                after_label.clone() // if there is no else, jump directly to the after block
            };

            // Append the conditional branch instruction to the block
            last_block.instructions.push(ir::Instruction::Conditional {
                condition: expression.explicate_atom(),
                then_label: then_label.clone(),
                else_label: else_label.clone(),
            });

            // Create the blocks for the true branch
            let mut then_blocks = {
                let mut blocks = vec![ir::Block::from_label(then_label)];
                let mut new_blocks = then.explicate_control(namer, blocks.last_mut().unwrap());
                blocks.append(&mut new_blocks);

                blocks
                    .last_mut()
                    .unwrap()
                    .instructions
                    .push(ir::Instruction::Goto { label: after_label.clone() });

                blocks
            };

            // If there is a false branch, create its blocks
            let mut else_blocks = if let Some(e) = else_expr {
                let mut blocks = vec![ir::Block::from_label(else_label)];
                let mut new_blocks = e.explicate_control(namer, blocks.last_mut().unwrap());
                blocks.append(&mut new_blocks);

                blocks
                    .last_mut()
                    .unwrap()
                    .instructions
                    .push(ir::Instruction::Goto { label: after_label.clone() });

                blocks
            } else {
                vec![]
            };

            let mut new_blocks = vec![];
            new_blocks.append(&mut then_blocks);
            new_blocks.append(&mut else_blocks);
            new_blocks.push(ir::Block::from_label(after_label)); // Create the block after

            new_blocks
        },

        ast::Expression::While { expression, block } => {
            // Create required labels
            let loop_label = namer.block_name(last_block, "loop");
            let body_label = namer.block_name(last_block, "body");
            let after_label = namer.block_name(last_block, "");

            // Append goto to the loop block
            last_block.instructions.push(
                ir::Instruction::Goto { label: loop_label.clone() }
            );

            // Create a block from the branch of the loop
            let loop_block = {
                let branch = ir::Instruction::Conditional {
                    condition: expression.explicate_atom(),
                    then_label: body_label.clone(),
                    else_label: after_label.clone(),
                };

                ir::Block {
                    name: loop_label.clone(),
                    instructions: vec![branch],
                }
            };

            // Create blocks for the body of the loop
            let mut body_blocks = {
                let mut blocks = vec![ir::Block::from_label(body_label)];
                let mut new_blocks = block.explicate_control(namer, blocks.last_mut().unwrap());
                blocks.append(&mut new_blocks);

                blocks
                    .last_mut()
                    .unwrap()
                    .instructions
                    .push(ir::Instruction::Goto { label: loop_label });

                blocks
            };

            let mut new_blocks = vec![loop_block];
            new_blocks.append(&mut body_blocks);
            new_blocks.push(ir::Block::from_label(after_label)); // Create the final block

            new_blocks
        },

        // Expressions in blocks are only allowed as return values from functions.
        ast::Expression::Block { statements, expression, .. } => {
            // Expressions are forbidden, there is no return values?
            if expression.is_some() {
                panic!("No returns are allowed from blocks!")
            }

            // Fold the statements into a list of blocks
            let mut blocks = vec![last_block.clone()];
            for s in statements {
                let mut new_blocks = s.explicate_control(namer, blocks.last_mut().unwrap());
                blocks.append(&mut new_blocks)
            }
            blocks.remove(0);

            blocks
        },

        // todo: discuss this
        // Everything else is assigned to an unused temporary variable
        e => {
            let var = namer.get_unused_id();
            let expr = e.explicate_expression();
            let assignment = ir::Instruction::Assignment { var, expr };

            last_block.instructions.push(assignment);
            vec![] // no new blocks are created
        }
    }}

    fn explicate_atom(self) -> ir::Atom { match self {
        ast::Expression::Variable { id } => ir::Atom::Variable { id },
        ast::Expression::Literal(val) => ir::Atom::Value(val),

        e => panic!("Expression should be an Atom! Found {:?} instead", e)
    }}

    fn explicate_expression(self) -> ir::Expression { match self {
        ast::Expression::UnaryOp { op, expr } => ir::Expression::Unary { op, atom: expr.explicate_atom() },
        ast::Expression::BinaryOp { lhs, op, rhs } => ir::Expression::Binary { lhs: lhs.explicate_atom(), op, rhs: rhs.explicate_atom() },
        ast::Expression::FunctionCall { id, args } => {
            let args = args.into_iter().map(|a| a.explicate_atom()).collect();
            ir::Expression::FunCall { id, args }
        }

        // In every other scenario it should be an Atom. (invariant)
        _ => ir::Expression::Atom(self.explicate_atom())
    }}
}

impl ast::Statement {
    fn explicate_control(self, namer: &mut Namer, last_block: &mut ir::Block) -> Vec<ir::Block> { match self {
        // Assignments and declarations turn into assignment instructions
        ast::Statement::Assignment { id, expression } |
        ast::Statement::Declaration { id, expression, .. } => {
            let instr = ir::Instruction::Assignment { var: id, expr: expression.explicate_expression() };
            last_block.instructions.push(instr);

            vec![] // No additional blocks were added, last_block is still the last block
        }

        // Expression can potentially turn into several blocks
        ast::Statement::Expression(expr) => expr.explicate_control(namer, last_block)
    }}
}
