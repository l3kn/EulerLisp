use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::symbol_table::Symbol;
use crate::{LispResult, Value};

pub struct Context {
    constants: RefCell<Vec<Value>>,
    constants_table: RefCell<HashMap<Symbol, usize>>,
    globals: RefCell<Vec<Value>>,
    globals_table: RefCell<HashMap<Symbol, usize>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            constants: RefCell::new(vec![]),
            constants_table: RefCell::new(HashMap::new()),
            globals: RefCell::new(vec![]),
            globals_table: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants.borrow()[index].clone()
    }

    pub fn lookup_constant_index(&self, name: Symbol) -> Option<usize> {
        self.constants_table.borrow().get(&name).map(|index| *index)
    }

    pub fn add_anonymous_constant(&self, value: Value) -> usize {
        let mut constants = self.constants.borrow_mut();
        let index = constants.iter().position(|x| *x == value);

        match index {
            Some(index) => index,
            None => {
                let index = constants.len();
                constants.push(value);
                index
            }
        }
    }

    pub fn add_constant(&self, name: Symbol, value: Value) -> usize {
        let index = self.add_anonymous_constant(value);
        self.constants_table.borrow_mut().insert(name, index);
        index
    }

    pub fn get_global(&self, index: usize) -> Value {
        self.globals.borrow()[index].clone()
    }

    pub fn set_global(&self, index: usize, value: Value) {
        self.globals.borrow_mut()[index] = value;
    }

    pub fn lookup_global_index(&self, name: Symbol) -> Option<usize> {
        self.globals_table.borrow().get(&name).map(|index| *index)
    }

    pub fn add_global(&self, name: Symbol, value: Value) -> usize {
        let mut globals = self.globals.borrow_mut();
        let index = globals.len();
        globals.push(value);
        self.globals_table.borrow_mut().insert(name, index);
        index
    }
}
