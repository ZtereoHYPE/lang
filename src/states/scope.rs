use crate::states::ast::{Identifier, Symbol, SymbolTable};
use itertools::Itertools;

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
