use std::collections::HashMap;

use crate::Symbol;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct SymbolTable {
    pub index: usize,
    mapping: HashMap<String, usize>,
    names: Vec<String>,
}

impl SymbolTable {
    pub fn insert(&mut self, key: &str) -> usize {
        if self.mapping.contains_key(key) {
            self.mapping[key]
        } else {
            let i = self.index;
            self.mapping.insert(key.to_string(), i);
            self.names.push(key.to_string());
            self.index += 1;

            i
        }
    }

    pub fn lookup(&self, symbol: Symbol) -> String {
        self.names[symbol].clone()
    }
}

#[test]
fn insert_and_lookup() {
    let mut st = SymbolTable::default();
    assert_eq!(st.insert("foo"), 0);
    assert_eq!(st.insert("bar"), 1);
    assert_eq!(st.insert("foo"), 0);
}
