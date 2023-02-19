use std::collections::HashMap;

use xlang_util::format::{Grouper, NodeDisplay, TreeDisplay};

use crate::const_value::{ConstValue, Type};

#[derive(Clone)]
pub enum ScopeValue {
    ConstValue(ConstValue),
    Record { members: HashMap<String, Type> },
}

impl NodeDisplay for ScopeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ScopeValue::ConstValue(_) => f.write_str("Constant Value"),
            ScopeValue::Record { .. } => f.write_str("Record"),
        }
    }
}

impl TreeDisplay for ScopeValue {
    fn num_children(&self) -> usize {
        match self {
            ScopeValue::ConstValue(c) => c.num_children(),
            ScopeValue::Record { .. } => 1,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ScopeValue::ConstValue(c) => c.child_at(index),
            ScopeValue::Record { members } => Some(members),
        }
    }
}

pub struct Scope {
    symbols: HashMap<String, ScopeValue>,
}

impl NodeDisplay for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Scope")
    }
}

impl TreeDisplay for Scope {
    fn num_children(&self) -> usize {
        self.symbols.len()
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay<()>> {
        None
    }

    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        let (name, item) = self.symbols.iter().nth(index).unwrap();
        Box::new(Grouper(name.clone(), item))
    }
}

#[derive(Clone)]
pub struct ScopeRef(usize, String);

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

    pub fn get_symbol_ref(&self, name: &str) -> Option<ScopeRef> {
        let found = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|scope| scope.1.symbols.get(name).map(|sc| (scope.0, name)));

        if let Some((ind, st)) = found {
            Some(ScopeRef(ind, st.to_string()))
        } else {
            None
        }
    }

    pub fn get_symbol(&self, scope_ref: &ScopeRef) -> Option<&ScopeValue> {
        if let Some(scp) = self.scopes.get(scope_ref.0) {
            if let Some(sym) = scp.symbols.get(&scope_ref.1) {
                return Some(sym);
            }
        }

        None
    }

    pub fn find_symbol(&self, name: &str) -> Option<&ScopeValue> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.symbols.get(name))
    }

    pub fn find_symbol_mut(&mut self, name: &str) -> Option<&mut ScopeValue> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.symbols.get_mut(name))
    }

    pub fn update_value(&mut self, name: &str, value: ScopeValue) -> Option<ScopeValue> {
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

impl NodeDisplay for ScopeManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Scope Manager")
    }
}

impl TreeDisplay for ScopeManager {
    fn num_children(&self) -> usize {
        self.scopes.len()
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        if let Some(scope) = self.scopes.get(index) {
            Some(scope)
        } else {
            None
        }
    }
}
