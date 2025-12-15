use std::process::exit;
use crate::ast::Item::Function;
use crate::ast::{Expression, Identifier, Item, Program, Statement, Symbol, SymbolTable};


#[derive(Clone)]
pub struct ScopeStack<'a> {
    pub(crate) stack: Vec<&'a SymbolTable>
}

impl<'a> ScopeStack<'a> {
    pub(crate) fn with_scope(&self, scope: &'a SymbolTable) -> Self {
        let mut stack = self.stack.clone();
        stack.push(scope);
        Self { stack }
    }

    pub(crate) fn resolve_symbol(&self, id: &Identifier) -> Option<Symbol> {
        // Find the topmost scope that contains the symbol, and map it to an Option<SymbolInfo> (cloned to avoid reference)
        self.stack
            .iter()
            .rev()
            .find_map(|s| s.get(id).cloned())
    }
}

struct SymbolError {
    error: String
}

impl SymbolError {
    fn new(error: String) -> Self {Self {error}}
}

pub fn resolve_symbols(program: &mut Program) {
    let stack = ScopeStack {
        stack: vec![],
    };

    if let Err(e) = program.populate_symbols(stack) {
        println!("Failed resolving symbols: {}", e.error);
        exit(-1); // todo: find a better solution than exiting here. Maybe propagate the error upwards?
    }
}

// This is not very elegant because the parent node is responsible for populating its own symbol table
// and thus needs to iterate over its children before propagating.
//
// Ideally, the children themselves would be responsible for "adding themselves" to the symbol table.
// However, that is annoying to do for 2 reasons:
//
// 1.   The functions need to have a global scope, before or after their declaration. This is so that
//      the first function's body can call the second function. This means all functions have to be added
//      to the table before recursing down the tree.
//
// 2.   Rust's borrow-checker rules make that a bit annoying. To be able to mutate the symbols on the tree,
//      the ScopeStack would somehow need to hold a mutable reference to the SymbolTable. While some
//      design changes could probably be made to work around this, I decided to just move forward for the
//      time being. Feel free to try and fix this.

// I could try having only 1 scope stack that is pushed and popped to. This way, it can hold mutable references,
// and we solve the performance issues.

impl Program {
    fn populate_symbols(&mut self, stack: ScopeStack) -> Result<(), SymbolError> {
        // Add all functions to the symbol list, as long as there's no collision
        for Function {id, ..} in &self.items {
            if self.symbols.contains_key(&id) {
                return Err(SymbolError::new(format!("Function with id {} already exists!", id.id)))
            }

            self.symbols.insert(id.clone(), Symbol::Unknown);
        }

        // Recursively populate the symbols down the tree, with the additional local scope
        for item in &mut self.items {
            item.populate_symbols(stack.with_scope(&self.symbols))?;
        }

        Ok(())
    }
}

impl Item {
    fn populate_symbols(&mut self, stack: ScopeStack) -> Result<(), SymbolError> { match self {
        Function { symbols, params, body, .. } => {
            // Add all parameters to the symbol list, as long as there's no collision
            for (id, _) in params {
                if symbols.contains_key(&id) {
                    return Err(SymbolError::new(format!("Function argument '{}' already exists!", id.id)))
                }

                symbols.insert(id.clone(), Symbol::Unknown);
            }

            // Recursively populate the symbols down the tree
            body.populate_symbols(stack.with_scope(symbols))
        }
    }}
}

impl Expression {
    fn populate_symbols(&mut self, stack: ScopeStack) -> Result<(), SymbolError> {
        match self {
            Expression::UnaryOp { expr, .. } => expr.populate_symbols(stack),

            Expression::BinaryOp { lhs, rhs, .. } => {
                lhs.populate_symbols(stack.clone())?;
                rhs.populate_symbols(stack)
            }

            Expression::FunctionCall { id, args, .. } => {
                if let None = stack.resolve_symbol(id) {
                    return Err(SymbolError::new(format!("Could not find find symbol '{}'", id.id)))
                }

                for arg in args {
                    arg.populate_symbols(stack.clone())?;
                }

                Ok(())
            }

            Expression::If { expression, then, else_expr } => {
                expression.populate_symbols(stack.clone())?;

                if let Some(expr) = else_expr {
                    expr.populate_symbols(stack.clone())?
                }

                then.populate_symbols(stack)
            }

            Expression::While { expression, block } => {
                expression.populate_symbols(stack.clone())?;
                block.populate_symbols(stack)
            },

            Expression::Block { symbols, statements, expression } => {
                for statement in statements {
                    match statement {
                        Statement::Assignment { id, expression } => {
                            expression.populate_symbols(stack.with_scope(symbols))?;
                            if let None = stack.with_scope(symbols).resolve_symbol(id) {
                                return Err(SymbolError::new(format!("Could not find symbol '{}'.", id.id)));
                            }
                        }

                        Statement::Declaration { id, expression, .. } => {
                            expression.populate_symbols(stack.with_scope(symbols))?;
                            if symbols.contains_key(&id) {
                                return Err(SymbolError::new(format!("Symbol '{}' already exists in the local scope!", id.id)));
                            }
                            symbols.insert(id.clone(), Symbol::Unknown);
                        }

                        Statement::Expression(expression) => {
                            expression.populate_symbols(stack.with_scope(symbols))?;
                        }
                    }
                }

                if let Some(expr) = expression {
                    return expr.populate_symbols(stack.with_scope(symbols))
                }

                Ok(())
            }

            Expression::Variable { id } => {
                if let None = stack.resolve_symbol(id) {
                    return Err(SymbolError::new(format!("Could not find symbol '{}'", id.id)));
                }

                Ok(())
            }

            _ => Ok(())
        }
    }
}
