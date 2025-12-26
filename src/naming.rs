use std::collections::HashMap;
use std::string::ToString;
use crate::states::ast::Identifier;
use crate::states::ir;

pub struct Namer {
    map: HashMap<String, u64>
}

impl Namer {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    fn get_increment_value(&mut self, str: &String) -> u64 {
        let value = *self.map.get(str).unwrap_or(&1u64);
        self.map.insert(*str, value + 1);
        value
    }

    pub fn block_name(&mut self, block: &ir::Block, block_type: &str) -> String {
        let fn_name = Self::get_fn_name(block);

        let block_name = if block_type.is_empty() {
            fn_name
        } else {
            &format!("{}-{}", fn_name, block_type)
        };

        let value = self.get_increment_value(block_name);

        format!("{}-{}", block_name, value)
    }

    const UNUSED: String = "_".to_string();
    pub fn get_unused_id(&mut self) -> Identifier {
        let value = self.get_increment_value(&Self::UNUSED);
        Identifier { id: format!("_{}", value) }
    }

    fn get_fn_name(block: &ir::Block) -> &String {
        block.name.split("-")[0]
    }
}