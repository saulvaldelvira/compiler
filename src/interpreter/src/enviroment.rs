use std::collections::{HashMap, VecDeque};

use ast::{declaration::VariableDecl, expr::LitValue};

struct Var {
    is_const: bool,
    value: LitValue,
}

pub struct Enviroment {
    variables: VecDeque<HashMap<Box<str>,Var>>,
}

impl Enviroment {
    pub fn new() -> Self {
        let mut global = VecDeque::new();
        global.push_back(HashMap::new());
        Self { variables: global }
    }
    pub fn define(&mut self, name: impl Into<Box<str>>, value: LitValue, decl: &VariableDecl) {
        let variables = self.variables.back_mut().unwrap();
        variables.insert(name.into(), Var { is_const: decl.is_const, value });
    }
    fn get(&mut self, name: &str) -> Option<&mut Var> {
        let mut i = self.variables.len() as i32 - 1;
        while i >= 0 {
            if self.variables[i as usize].contains_key(name) {
                return self.variables[i as usize].get_mut(name);
            }
            i -= 1;
        }
        None
    }
    pub fn get_val(&mut self, name: &str) -> Option<&mut LitValue> {
        self.get(name).map(|var| &mut var.value)
    }
    pub fn is_const(&mut self, name: &str) -> bool {
        self.get(name).map(|var| var.is_const).unwrap_or(false)
    }
    pub fn set(&mut self) {
        self.variables.push_back(HashMap::new());
    }
    pub fn reset(&mut self) {
        self.variables.pop_back();
    }
}
