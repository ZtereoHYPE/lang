use crate::states::ast::Identifier;
use crate::states::ir;
use std::collections::HashMap;
use std::string::ToString;

pub struct Namer {
    current_fn_name: String,
    map: HashMap<String, u64>
}

impl Namer {
    pub fn new() -> Self {
        Self { current_fn_name: "".to_string(), map: HashMap::new() }
    }

    pub fn set_fn_name(&mut self, name: String) {
        self.current_fn_name = name;
    }

    pub fn block_name(&mut self, block_type: &str) -> String {
        let block_name = if block_type.is_empty() {
            self.current_fn_name.clone()
        } else {
            format!("{}-{}", self.current_fn_name, block_type)
        };

        let value = self.get_increment_value(&block_name);
        format!("{}-{}", block_name, value)
    }

    pub fn get_unused_id(&mut self) -> Identifier {
        let value = self.get_increment_value("_");
        Identifier { id: format!("_{}", value) }
    }

    fn get_increment_value(&mut self, str: &str) -> u64 {
        let value = *self.map.get(str).unwrap_or(&1u64);
        self.map.insert(str.to_string(), value + 1);
        value
    }
}