use crate::interpreter::Value;
use crate::representations::ast::{Identifier, Operator};
use crate::representations::ir;
use crate::representations::ir::{Atom, Expression, Terminal};

#[derive(Debug, Clone)]
pub struct StackVar {
    pub id: Identifier,
    pub value: Value,
    pub alive: bool
}

#[derive(Debug, Clone)]
pub struct IrAddress<'a> {
    pub function: &'a ir::Function,
    pub block: &'a ir::Block,
    pub statement: usize
}

#[derive(Debug, Clone)]
pub struct CallFrame<'a> {
    pub ip: IrAddress<'a>, // instruction pointer
    pub locals: Vec<StackVar>,
    assignee: Identifier
}

pub struct IrInterpreter<'a> {
    program: &'a ir::Program,
    stack: Vec<CallFrame<'a>>,
    pub output: Value
}

impl<'a> IrInterpreter<'a> {
    pub fn new(program: &'a ir::Program) -> Self {
        let main_f = program.functions
            .iter()
            .find(|f| f.name.id == "main")
            .expect("Failed to locate prorgram entrypoint");

        let block = main_f.blocks
            .get(&main_f.entrypoint)
            .expect("A function's entrypoint must exist");

        let stack_root = CallFrame {
            locals: vec![],
            ip: IrAddress {
                function: main_f,
                block,
                statement: 0,
            },
            assignee: Identifier { id: "".to_string() },
        };

        IrInterpreter {
            program,
            stack: vec![stack_root],
            output: Value::Unit,
        }
    }

    pub fn run(mut self) -> Value {
        while !self.stack.is_empty() {
            self.execute_step();
        }

        self.output
    }

    pub fn execute_step(&mut self) {
        let statement = self.frame().ip.statement;
        let block = self.frame().ip.block;
        assert!(statement <= block.assignments.len(), "Executing statement outside of block!");

        let is_terminal = statement == block.assignments.len();

        if is_terminal {
            let terminal = &block.terminal;
            self.execute_terminal(terminal);

        } else {
            let assignment = block.assignments.get(statement).unwrap();
            self.execute_asssignment(assignment);
        }
        self.mark_dead_locals();
    }

    pub fn state(&self) -> Option<&CallFrame<'a>> {
        if self.stack.is_empty() {
            None
        } else {
            Some(self.frame())
        }
    }

    fn execute_asssignment(&mut self, ass: &ir::Assignment) {
        let (ass_id, expr) = ass;

        let value = match expr {
            Expression::Unary { op, atom } => match (op, self.evaluate_atom(atom)) {
                (Operator::Negated, Value::Int(i))  => Value::Int(-i),
                (Operator::Not,     Value::Bool(b)) => Value::Bool(!b),
                _ => unreachable!("Other operators cannot exist in unary expressions, or cannot be used with other data types")
            }

            Expression::Binary { lhs, op, rhs } => match (self.evaluate_atom(lhs), op, self.evaluate_atom(rhs)) {
                (Value::Int(i1), Operator::Plus,                Value::Int(i2)) => Value::Int(i1 + i2),
                (Value::Int(i1), Operator::Minus,               Value::Int(i2)) => Value::Int(i1 - i2),
                (Value::Int(i1), Operator::Times,               Value::Int(i2)) => Value::Int(i1 * i2),
                (Value::Int(i1), Operator::GreaterThan,         Value::Int(i2)) => Value::Bool(i1 > i2),
                (Value::Int(i1), Operator::GreaterOrEqualThan,  Value::Int(i2)) => Value::Bool(i1 >= i2),
                (Value::Int(i1), Operator::LessThan,            Value::Int(i2)) => Value::Bool(i1 < i2),
                (Value::Int(i1), Operator::LessOrEqualThan,     Value::Int(i2)) => Value::Bool(i1 <= i2),
                (Value::Int(i1), Operator::Equal,               Value::Int(i2)) => Value::Bool(i1 == i2),
                (Value::Int(i1), Operator::Different,           Value::Int(i2)) => Value::Bool(i1 != i2),
                (Value::Int(i1), Operator::Divided,             Value::Int(i2)) => {
                    if i2 == 0 { panic!("Division by zero in function {}", self.frame().ip.function.name.id) }
                    Value::Int(i1 / i2)
                },
                _ => unreachable!("Other operators cannot exist in binary expressions, or cannot be used with other data types")
            }

            Expression::FunCall { id, args } => {
                let function = self.program.functions
                    .iter()
                    .find(|f| f.name == *id)
                    .expect("Failed to find function to call!");

                let args = args.iter().map(|a| self.evaluate_atom(a)).collect();

                self.call_function(function, args, ass_id.clone());

                return;
            }

            Expression::Atom(a) => self.evaluate_atom(a)
        };

        self.insert_local(ass_id.clone(), value);
        self.increment_ip();
    }

    fn execute_terminal(&mut self, term: &ir::Terminal) {
        match term {
            Terminal::Goto { label } => {
                let block = &self.block(label);
                self.jmp_to_block(block);
            },

            Terminal::Conditional { condition, then_label, else_label } => {
                let Value::Bool(boolean) = self.evaluate_atom(condition) else { unreachable!("Conditions must be booleans!") };

                if boolean {
                    let then_block = &self.block(then_label);
                    self.jmp_to_block(then_block);
                } else {
                    let else_block = &self.block(else_label);
                    self.jmp_to_block(else_block);
                }
            }

            Terminal::Return(atom) => {
                let value = self.evaluate_atom(atom);
                self.return_function(value)
            }
        }
    }

    fn evaluate_atom(&self, atom: &ir::Atom) -> Value {
        match atom {
            Atom::Variable { id } => self.local(id),
            Atom::Value(literal) => literal.into()
        }
    }

    fn call_function(&mut self, function: &'a ir::Function, args: Vec<Value>, assignee: Identifier) {
        let locals = function.params
            .iter()
            .cloned()
            .zip(args)
            .map(|(id, value)| StackVar { id, value, alive: true })
            .collect();

        let block = function.blocks
            .get(&function.entrypoint)
            .expect("A function's entrypoint must exist");

        let new_frame = CallFrame {
            locals,
            assignee,
            ip: IrAddress {
                block,
                function,
                statement: 0
            },
        };

        self.stack.push(new_frame);
    }

    fn return_function(&mut self, value: Value) {
        let prev = self.stack.pop().expect("Returning from a function with no stack frame!");

        // If we are done executing the code set the output instead
        if self.stack.is_empty() {
            self.output = value;
        } else {
            self.insert_local(prev.assignee, value);
            self.increment_ip();
        }
    }

    fn jmp_to_block(&mut self, block: &'a ir::Block) {
        self.frame_mut().ip.block = block;
        self.frame_mut().ip.statement = 0;
    }

    // Helper methods
    fn increment_ip(&mut self) {
        self.frame_mut().ip.statement += 1;
    }

    fn local(&self, id: &Identifier) -> Value {
        let local = self.frame().locals
            .iter()
            .find(|v| v.id == *id)
            .expect(&format!("Could not find {} in locals!", id.id));

        local.value
    }

    fn insert_local(&mut self, id: Identifier, value: Value) {
        let idx = self.frame().locals
            .iter()
            .enumerate()
            .find_map(|(idx, l)| if l.id == id { Some(idx) } else { None });

        if let Some(idx) = idx {
            self.frame_mut().locals[idx].value = value;
            self.frame_mut().locals[idx].alive = true;
        } else {
            let var = StackVar { id, value, alive: true };
            self.frame_mut().locals.push(var);
        }
    }

    fn mark_dead_locals(&mut self) {
        if self.stack.is_empty() { return; }

        let live = &self.frame().ip.block.liveness[self.frame().ip.statement];

        self.frame_mut().locals
            .iter_mut()
            .filter(|l| !live.contains(&l.id))
            .for_each(|l| l.alive = false);
    }

    fn frame(&self) -> &CallFrame<'a> {
        &self.stack.last().expect("The stack should never be empty")
    }

    fn frame_mut(&mut self) -> &mut CallFrame<'a> {
        self.stack.last_mut().expect("The stack should never be empty")
    }

    fn block(&self, name: &String) -> &'a ir::Block {
        self.frame().ip.function.blocks
            .get(name)
            .expect(&format!("Could not find block {} in function!", name))
    }
}
