use std::collections::{HashMap, VecDeque};

use ast::{declaration::VariableDecl, expr::LitValue};
use session::Symbol;

struct Var {
    is_const: bool,
    value: LitValue,
}

#[derive(Default)]
struct EnviromentScope {
    variables: HashMap<Symbol, Var>,
}

impl EnviromentScope {
    fn new() -> Self {
        Self::default()
    }
}

pub struct Enviroment {
    scopes: VecDeque<EnviromentScope>,
}

impl Enviroment {
    pub fn new() -> Self {
        let mut global = VecDeque::new();
        global.push_back(EnviromentScope::new());
        Self { scopes: global }
    }
    pub fn define_var(&mut self, name: Symbol, value: LitValue, decl: &VariableDecl) {
        let scope = self.scopes.back_mut().unwrap();
        scope.variables.insert(name, Var { is_const: decl.is_const, value });
    }
    fn get_var(&mut self, name: &Symbol) -> Option<&mut Var> {
        let mut i = self.scopes.len() as i32 - 1;
        while i >= 0 {
            if self.scopes[i as usize].variables.contains_key(name) {
                return self.scopes[i as usize].variables.get_mut(name);
            }
            i -= 1;
        }
        None
    }
    pub fn get_val(&mut self, name: &Symbol) -> Option<&mut LitValue> {
        self.get_var(name).map(|var| &mut var.value)
    }
    pub fn is_const(&mut self, name: &Symbol) -> bool {
        self.get_var(name).map(|var| var.is_const).unwrap_or(false)
    }
    pub fn set(&mut self) {
        self.scopes.push_back(EnviromentScope::new());
    }
    pub fn reset(&mut self) {
        self.scopes.pop_back();
    }
}
