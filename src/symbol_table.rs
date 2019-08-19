use std::collections::HashMap;

use crate::Symbol;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct SymbolTable {
    pub index: usize,
    mapping: HashMap<String, usize>,
    names: Vec<String>,
}

// Reserved
pub const DO: usize = 0;
pub const FN: usize = 1;
pub const QUOTE: usize = 2;
pub const DEFSYNTAX: usize = 3;
pub const DEFCONST: usize = 4;
pub const DEF: usize = 5;
pub const SET: usize = 6;
pub const IF: usize = 7;
pub const LET: usize = 8;
// Monadic Builtins
pub const INC: usize = 9;
pub const DEC: usize = 10;
pub const FST: usize = 11;
pub const RST: usize = 12;
pub const NOT: usize = 13;
pub const IS_ZERO: usize = 14;
pub const IS_NIL: usize = 15;
pub const NEG: usize = 16;
// Syntax
pub const ELLIPSIS: usize = 17;
// Binary Primitives
pub const BIN_ADD: usize = 18;
pub const BIN_SUB: usize = 19;
pub const BIN_MUL: usize = 20;
pub const BIN_DIV: usize = 21;
pub const DIV: usize = 22;
pub const POW: usize = 23;
pub const SQRT: usize = 24;

impl SymbolTable {
    /// Seed the table with built-in symbols that are used in the compiler
    pub fn seed(&mut self) {
        self.insert("do");
        self.insert("fn");
        self.insert("quote");
        self.insert("defsyntax");
        self.insert("defconst");
        self.insert("def");
        self.insert("set!");
        self.insert("if");
        self.insert("let");
        // Monadic Builtins
        self.insert("inc");
        self.insert("dec");
        self.insert("fst");
        self.insert("rst");
        self.insert("not");
        self.insert("zero?");
        self.insert("nil?");
        self.insert("neg");
        // Syntax
        self.insert("...");
        // Binary Primitives
        self.insert("bin+");
        self.insert("bin-");
        self.insert("bin*");
        self.insert("bin/");
        self.insert("div");
        self.insert("pow");
        self.insert("sqrt");
    }

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
