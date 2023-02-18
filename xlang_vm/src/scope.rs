use std::collections::HashMap;

use crate::const_value::ConstValue;


pub struct Scope {
    symbols: HashMap<String, ConstValue>,
}



pub struct ScopeManager {
    scopes: Vec<Scope>,
}

impl ScopeManager {
    pub fn new() -> ScopeManager {
        ScopeManager {
            scopes: Vec::with_capacity(10),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
        });
    }

    pub fn pop_scope(&mut self) -> Scope {
        self.scopes.remove(self.scopes.len() - 1)
    }

    pub fn find_symbol(&self, name: &str) -> Option<&ConstValue> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.symbols.get(name))
    }

    pub fn find_symbol_mut(&mut self, name: &str) -> Option<&mut ConstValue> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.symbols.get_mut(name))
    }

    pub fn update_value(&mut self, name: &str, value: ConstValue) -> Option<ConstValue> {
        if let Some(sym) = self.find_symbol_mut(name) {
            let old_sym = sym.clone();
            *sym = value;
            return Some(old_sym);
        }

        if let Some(scp) = self.scopes.last_mut() {
            scp.symbols.insert(name.to_string(), value);
        }

        None
    }
}
