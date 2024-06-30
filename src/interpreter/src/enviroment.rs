use std::{collections::{HashMap, VecDeque}, usize};

use ast::expr::LitValue;

pub struct Enviroment {
    variables: VecDeque<HashMap<String,LitValue>>,
}

impl Enviroment {
    pub fn new() -> Self {
        let mut global = VecDeque::new();
        global.push_back(HashMap::new());
        Self { variables: global }
    }
    pub fn define(&mut self, name: impl Into<String>, value: LitValue) {
        let current = self.variables.back_mut().unwrap();
        current.insert(name.into(), value);
    }
    pub fn get(&mut self, name: &str) -> Option<&mut LitValue> {
        let mut i = self.variables.len() as i32 - 1;
        while i >= 0 {
            if self.variables[i as usize].contains_key(name) {
                return self.variables[i as usize].get_mut(name);
            }
            i -= 1;
        }
        None
    }
    pub fn set(&mut self) {
        self.variables.push_back(HashMap::new());
    }
    pub fn reset(&mut self) {
        self.variables.pop_back();
    }
}
