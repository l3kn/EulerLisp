use std::collections::HashMap;

use ::Symbol;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable {
    pub index: usize,
    mapping: HashMap<String, usize>,
    names: Vec<String>
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { index: 0, mapping: HashMap::new(), names: Vec::new() }
    }

    pub fn insert(&mut self, key: &String) -> usize {
        if self.mapping.contains_key(key) {
            *self.mapping.get(key).unwrap()
        } else {
            let i = self.index;
            self.mapping.insert(key.clone(), i);
            self.names.push(key.clone());
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
    let mut st = SymbolTable::new();
    assert_eq!(st.insert("foo"), 0);
    assert_eq!(st.insert("bar"), 1);
    assert_eq!(st.insert("foo"), 0);
}
