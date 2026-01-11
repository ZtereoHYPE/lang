use std::collections::HashMap;
use crate::states::ast::{Expression, Function, Literal, Operator, Program, Statement};

/// runtime value
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
}

/// environment that holds variable bindings
type Environment = HashMap<String, Value>;

pub type InterpretResult = Result<Value, String>;

/// interprets the entire program, starting from main
pub fn interpret(program: &Program) -> InterpretResult {
    let mut interpreter = Interpreter::new(program);
    interpreter.run()
}

/// interpreter struct containing the program and functions
struct Interpreter<'a> {
    program: &'a Program,
    functions: HashMap<String, &'a Function>,
}

impl<'a> Interpreter<'a> {
    fn new(program: &'a Program) -> Self {
        let mut functions = HashMap::new();
        for f in &program.functions {
            functions.insert(f.id.id.clone(), f);
        }
        Interpreter { program, functions }
    }

    /// run the interpreter beginning from main
    fn run(&mut self) -> InterpretResult {
        self.call_function("main", vec![])
    }

    /// call a function with arguments
    fn call_function(&mut self, name: &str, args: Vec<Value>) -> InterpretResult {
        match name {
            "print" => {
                if let Some(val) = args.first() {
                    println!("{}", self.value_to_string(val));
                }
                return Ok(Value::Unit);
            }
            "print_int" => {
                if let Some(Value::Int(n)) = args.first() {
                    println!("{}", n);
                }
                return Ok(Value::Unit);
            }
            "print_bool" => {
                if let Some(Value::Bool(b)) = args.first() {
                    println!("{}", b);
                }
                return Ok(Value::Unit);
            }
            "read_int" => {
                // returns a default value rn, ideally we take a value from stdin
                return Ok(Value::Int(0));
            }
            _ => {}
        }

        let f = self.functions.get(name)
            .ok_or_else(|| format!("function '{}' not found", name))?
            .clone();

        // new environment with function parameters
        let mut env = Environment::new();
        for (i, (param_id, _)) in f.params.iter().enumerate() {
            if let Some(arg) = args.get(i) {
                env.insert(param_id.id.clone(), arg.clone());
            } else {
                return Err(format!("missing argument {} for function '{}'", i, name));
            }
        }

        self.eval_expression(&f.body, &mut env)
    }

    /// evaluat expression in the environment
    fn eval_expression(&mut self, expr: &Expression, env: &mut Environment) -> InterpretResult {
        match expr {
            Expression::Literal(lit) => self.eval_literal(lit),

            Expression::Variable { id } => {
                env.get(&id.id)
                    .cloned()
                    .ok_or_else(|| format!("variable '{}' not found", id.id))
            }

            Expression::UnaryOp { op, expr } => {
                let val = self.eval_expression(expr, env)?;
                self.eval_unary_op(op, val)
            }

            Expression::BinaryOp { lhs, op, rhs } => {
                let left = self.eval_expression(lhs, env)?;
                let right = self.eval_expression(rhs, env)?;
                self.eval_binary_op(op, left, right)
            }

            Expression::FunctionCall { id, args } => {
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.eval_expression(arg, env)?);
                }
                self.call_function(&id.id, evaluated_args)
            }

            Expression::If { expression, then, else_expr } => {
                let condition = self.eval_expression(expression, env)?;
                match condition {
                    Value::Bool(true) => self.eval_expression(then, env),
                    Value::Bool(false) => {
                        if let Some(else_branch) = else_expr {
                            self.eval_expression(else_branch, env)
                        } else {
                            Ok(Value::Unit)
                        }
                    }
                    _ => Err("condition must be bool".to_string()),
                }
            }

            Expression::While { expression, block } => {
                loop {
                    let condition = self.eval_expression(expression, env)?;
                    match condition {
                        Value::Bool(true) => {
                            self.eval_expression(block, env)?;
                        }
                        Value::Bool(false) => break,
                        _ => return Err("while condition must be bool".to_string()),
                    }
                }
                Ok(Value::Unit)
            }

            Expression::Block { symbols: _, statements, expression } => {
                // create a new scope(cloned)
                let mut block_env = env.clone();

                for stmt in statements {
                    self.eval_statement(stmt, &mut block_env)?;
                }

                // return final expression
                if let Some(expr) = expression {
                    self.eval_expression(expr, &mut block_env)
                } else {
                    Ok(Value::Unit)
                }
            }
        }
    }

    fn eval_statement(&mut self, stmt: &Statement, env: &mut Environment) -> InterpretResult {
        match stmt {
            Statement::Declaration { id, ty: _, expression } => {
                let val = self.eval_expression(expression, env)?;
                env.insert(id.id.clone(), val);
                Ok(Value::Unit)
            }

            Statement::Assignment { id, expression } => {
                let val = self.eval_expression(expression, env)?;
                if env.contains_key(&id.id) {
                    env.insert(id.id.clone(), val);
                    Ok(Value::Unit)
                } else {
                    Err(format!("cannot assign to undeclared variable '{}'", id.id))
                }
            }

            Statement::Expression(expr) => {
                self.eval_expression(expr, env)?;
                Ok(Value::Unit)
            }
        }
    }

    fn eval_literal(&self, lit: &Literal) -> InterpretResult {
        match lit {
            Literal::Int(n) => Ok(Value::Int(*n)),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::Unit() => Ok(Value::Unit),
        }
    }

    fn eval_unary_op(&self, op: &Operator, val: Value) -> InterpretResult {
        match (op, val) {
            (Operator::Negated, Value::Int(n)) => Ok(Value::Int(-n)),
            (Operator::Minus, Value::Int(n)) => Ok(Value::Int(-n)),
            (Operator::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("invalid unary operation: {:?}", op)),
        }
    }

    fn eval_binary_op(&self, op: &Operator, left: Value, right: Value) -> InterpretResult {
        match (op, left, right) {
            (Operator::Plus, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            (Operator::Minus, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            (Operator::Times, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
            (Operator::Divided, Value::Int(l), Value::Int(r)) => {
                if r == 0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(Value::Int(l / r))
                }
            }
            (Operator::GreaterThan, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
            (Operator::GreaterOrEqualThan, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
            (Operator::LessThan, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
            (Operator::LessOrEqualThan, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
            (Operator::Equal, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l == r)),
            (Operator::Different, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l != r)),
            (Operator::And, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
            (Operator::Or, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
            (Operator::Xor, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l ^ r)),
            (Operator::Equal, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
            (Operator::Different, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),

            _ => Err(format!("invalid binary operation: {:?}", op)),
        }
    }

    fn value_to_string(&self, val: &Value) -> String {
        match val {
            Value::Int(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Unit => "()".to_string(),
        }
    }
}

pub fn interpret_to_string(program: &Program) -> String {
    match interpret(program) {
        Ok(val) => match val {
            Value::Int(n) => format!("result: {}", n),
            Value::Bool(b) => format!("result: {}", b),
            Value::Unit => "result: ()".to_string(),
        },
        Err(e) => format!("error: {}", e),
    }
}

/// interpreter for TUI display
#[derive(Debug, Clone)]
pub struct StepInterpreter<'a> {
    program: &'a Program,
    functions: HashMap<String, &'a Function>,
    call_stack: Vec<CallFrame>,
    current_env: Environment,
    step_count: usize,
    finished: bool,
    result: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub function_name: String,
    pub env: Environment,
}

#[derive(Debug, Clone)]
pub struct StepResult {
    pub step: usize,
    pub description: String,
    pub current_function: String,
    pub environment: HashMap<String, String>,
    pub finished: bool,
    pub result: Option<String>,
}

impl<'a> StepInterpreter<'a> {
    pub fn new(program: &'a Program) -> Self {
        let mut functions = HashMap::new();
        for f in &program.functions {
            functions.insert(f.id.id.clone(), f);
        }
        
        StepInterpreter {
            program,
            functions,
            call_stack: vec![],
            current_env: Environment::new(),
            step_count: 0,
            finished: false,
            result: None,
        }
    }

    /// current state for display
    pub fn get_state(&self) -> StepResult {
        let current_function = self.call_stack.last()
            .map(|f| f.function_name.clone())
            .unwrap_or_else(|| "none".to_string());

        let environment: HashMap<String, String> = self.current_env
            .iter()
            .map(|(k, v)| (k.clone(), format!("{:?}", v)))
            .collect();

        StepResult {
            step: self.step_count,
            description: format!("step {}", self.step_count),
            current_function,
            environment,
            finished: self.finished,
            result: self.result.as_ref().map(|v| format!("{:?}", v)),
        }
    }

    pub fn is_finished(&self) -> bool {
        self.finished
    }
}
