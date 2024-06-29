use std::collections::HashMap;

use ast::expr::LitValue;

pub struct Enviroment {
    variables: HashMap<String,LitValue>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self { variables: HashMap::new() }
    }
    pub fn define(&mut self, name: impl Into<String>, value: LitValue) {
        self.variables.insert(name.into(), value);
    }
    pub fn get(&self, name: &str) -> Option<&LitValue> {
        self.variables.get(name)
    }
}
