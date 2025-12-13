use std::collections::HashMap;
use crate::ast::{Expression, Identifier, Program, Type};
use crate::ast::Item::Function;


struct FunctionInfo {
    ty: Type,
    params: Vec<(Identifier, Type)>
}

struct VarInfo {
    ty: Type
}

fn populateFunctions(ast: &Program) -> HashMap<Identifier, FunctionInfo> {
    ast.items
        .iter()
        .cloned()
        .filter_map(|i| match i {
            Function {id, params, ty, ..}
                => Some((id, FunctionInfo {params, ty})),
        })
        .collect()
}
