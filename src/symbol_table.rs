//! Symbol Table for translating between usize symbols and their string names.
//!
//! To avoid passing a reference to the symbol to every struct that needs to
//! match on symbols (e.g. the compiler and syntax rules), a few common symbols
//! are available as constants (so that they can be used in `match`).

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable {
    index: usize,
    mapping: HashMap<String, usize>,
    names: Vec<String>,
}

thread_local! {
    /// A non-thread-safe shared symbol table to avoid passing
    /// Rc<RefCell<>>s of it to every struct / function that needs it.
    ///
    /// Used by:
    /// - builtin print/println
    /// - error printing
    /// - lexer / parser
    pub static SYMBOL_TABLE: RefCell<SymbolTable> = RefCell::new(SymbolTable::new());
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(usize);

impl Symbol {
    pub fn intern(name: &str) -> Self {
        SYMBOL_TABLE.with(|s| s.borrow_mut().insert(name))
    }

    pub fn string(&self) -> String {
        SYMBOL_TABLE.with(|s| s.borrow().lookup(*self))
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        SYMBOL_TABLE.with(|s| {
            let name = s.borrow().lookup(*self);
            write!(f, "'{}", name)
        })
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        SYMBOL_TABLE.with(|s| {
            let name = s.borrow().lookup(*self);
            write!(f, "Symbol({})", name)
        })
    }
}
// Reserved
// NOTE: `compiler/mod.rs` assumes that all symbols `< LET` are reserved
pub const DO: Symbol = Symbol(0);
pub const FN: Symbol = Symbol(1);
pub const QUOTE: Symbol = Symbol(2);
pub const DEFSYNTAX: Symbol = Symbol(3);
pub const DEFCONST: Symbol = Symbol(4);
pub const DEF: Symbol = Symbol(5);
pub const SET: Symbol = Symbol(6);
pub const IF: Symbol = Symbol(7);
// Macros
pub const LET: Symbol = Symbol(8);
// Monadic Builtins
pub const INC: Symbol = Symbol(9);
pub const DEC: Symbol = Symbol(10);
pub const FST: Symbol = Symbol(11);
pub const RST: Symbol = Symbol(12);
pub const NOT: Symbol = Symbol(13);
pub const IS_ZERO: Symbol = Symbol(14);
pub const IS_NIL: Symbol = Symbol(15);
pub const NEG: Symbol = Symbol(16);
// Syntax
pub const ELLIPSIS: Symbol = Symbol(17);
// Binary Primitives
pub const ADD: Symbol = Symbol(18);
pub const SUB: Symbol = Symbol(19);
pub const MUL: Symbol = Symbol(20);
pub const DIV: Symbol = Symbol(21);
pub const BIN_EQ: Symbol = Symbol(22);
pub const BIN_LT: Symbol = Symbol(23);
pub const BIN_GT: Symbol = Symbol(24);
pub const BIN_LTE: Symbol = Symbol(25);
pub const BIN_GTE: Symbol = Symbol(26);
pub const BIN_EQUAL: Symbol = Symbol(27);
pub const NE: Symbol = Symbol(28);
pub const CONS: Symbol = Symbol(29);
pub const IDIV: Symbol = Symbol(30);
pub const MOD: Symbol = Symbol(31);
pub const VECTOR_REF: Symbol = Symbol(32);
// Ternary Primitives
pub const VECTOR_SET: Symbol = Symbol(33);
// Others, for constant folding
pub const POW: Symbol = Symbol(34);
pub const SQRT: Symbol = Symbol(35);

pub const CALL_CC: Symbol = Symbol(36);
pub const APPLY: Symbol = Symbol(37);
pub const EVAL: Symbol = Symbol(38);
pub const MACRO: Symbol = Symbol(39);
pub const READ: Symbol = Symbol(40);

impl SymbolTable {
    /// Seed the table with built-in symbols that are used in the compiler
    pub fn new() -> Self {
        let mut st = Self {
            index: 0,
            mapping: HashMap::new(),
            names: Vec::new(),
        };

        st.insert("do");
        st.insert("fn");
        st.insert("quote");
        st.insert("defsyntax");
        st.insert("defconst");
        st.insert("def");
        st.insert("set!");
        st.insert("if");
        st.insert("let");
        // Monadic Builtins
        st.insert("inc");
        st.insert("dec");
        st.insert("fst");
        st.insert("rst");
        st.insert("not");
        st.insert("zero?");
        st.insert("nil?");
        st.insert("neg");
        // Syntax
        st.insert("...");
        // Binary Primitives
        st.insert("__+");
        st.insert("__-");
        st.insert("__*");
        st.insert("__/");
        st.insert("__bin=");
        st.insert("__bin<");
        st.insert("__bin>");
        st.insert("__bin<=");
        st.insert("__bin>=");
        st.insert("__binequal?");
        st.insert("!=");
        st.insert("cons");
        st.insert("div");
        st.insert("%");
        st.insert("vector-ref");
        st.insert("vector-set!");

        st.insert("pow");
        st.insert("sqrt");

        st.insert("call/cc");
        st.insert("__apply");
        st.insert("eval");
        st.insert("macro");
        st.insert("read");

        st
    }

    pub fn insert(&mut self, key: &str) -> Symbol {
        if self.mapping.contains_key(key) {
            Symbol(self.mapping[key])
        } else {
            let i = self.index;
            self.mapping.insert(key.to_string(), i);
            self.names.push(key.to_string());
            self.index += 1;

            Symbol(i)
        }
    }

    pub fn lookup(&self, symbol: Symbol) -> String {
        self.names[symbol.0].clone()
    }
}

#[test]
fn insert_and_lookup() {
    let mut st = SymbolTable::default();
    assert_eq!(st.insert("foo"), 0);
    assert_eq!(st.insert("bar"), 1);
    assert_eq!(st.insert("foo"), 0);
}
