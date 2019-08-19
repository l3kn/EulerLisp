use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::symbol_table::SymbolTable;
use crate::vm::VM;
use crate::{Arity, LispResult, Symbol, Value};
use crate::{LispFn1, LispFn2, LispFn3, LispFnN, LispFnType};

mod bignum;
mod bitwise;
mod comparison;
mod list;
mod math;
mod misc;
mod primes;
mod string;
mod types;

// The difference between builtins and special forms is
// that special forms choose if they want to eval their arguments themselves,
// builtins are called with evaluated arguments

#[derive(Clone, Default)]
pub struct BuiltinRegistry {
    symbol_table: Rc<RefCell<SymbolTable>>,
    pub mapping: HashMap<Symbol, (LispFnType, u16, Arity)>,
    // Used for prettyprinting of call instructions
    inverse_mapping: HashMap<(LispFnType, u16), Symbol>,
    fns_1: Vec<LispFn1>,
    fns_2: Vec<LispFn2>,
    fns_3: Vec<LispFn3>,
    fns_n: Vec<LispFnN>,
}

impl BuiltinRegistry {
    pub fn new(symbol_table: Rc<RefCell<SymbolTable>>) -> Self {
        Self {
            symbol_table,
            mapping: HashMap::new(),
            inverse_mapping: HashMap::new(),
            fns_1: Vec::new(),
            fns_2: Vec::new(),
            fns_3: Vec::new(),
            fns_n: Vec::new(),
        }
    }

    pub fn register_var(&mut self, name: &str, f: LispFnN, arity: Arity) {
        let symbol = self.symbol_table.borrow_mut().insert(name);
        self.mapping.insert(
            symbol,
            (LispFnType::Variadic, self.fns_n.len() as u16, arity),
        );
        self.inverse_mapping
            .insert((LispFnType::Variadic, self.fns_n.len() as u16), symbol);
        self.fns_n.push(f);
    }

    pub fn register1(&mut self, name: &str, f: LispFn1) {
        let symbol = self.symbol_table.borrow_mut().insert(name);
        self.mapping.insert(
            symbol,
            (LispFnType::Fixed1, self.fns_1.len() as u16, Arity::Exact(1)),
        );
        self.inverse_mapping
            .insert((LispFnType::Fixed1, self.fns_1.len() as u16), symbol);
        self.fns_1.push(f);
    }

    pub fn register2(&mut self, name: &str, f: LispFn2) {
        let symbol = self.symbol_table.borrow_mut().insert(name);
        self.mapping.insert(
            symbol,
            (LispFnType::Fixed2, self.fns_2.len() as u16, Arity::Exact(2)),
        );
        self.inverse_mapping
            .insert((LispFnType::Fixed2, self.fns_2.len() as u16), symbol);
        self.fns_2.push(f);
    }

    pub fn register3(&mut self, name: &str, f: LispFn3) {
        let symbol = self.symbol_table.borrow_mut().insert(name);
        self.mapping.insert(
            symbol,
            (LispFnType::Fixed3, self.fns_3.len() as u16, Arity::Exact(3)),
        );
        self.inverse_mapping
            .insert((LispFnType::Fixed3, self.fns_3.len() as u16), symbol);
        self.fns_3.push(f);
    }

    pub fn lookup_name(&self, typ: LispFnType, id: u16) -> Symbol {
        self.inverse_mapping[&(typ, id)]
    }

    pub fn contains_key(&self, key: Symbol) -> bool {
        self.mapping.contains_key(&key)
    }

    pub fn call_1(&self, idx: usize, arg: Value, vm: &VM) -> LispResult<Value> {
        self.fns_1[idx](arg, vm)
    }

    pub fn call_2(&self, idx: usize, arg1: Value, arg2: Value, vm: &VM) -> LispResult<Value> {
        self.fns_2[idx](arg1, arg2, vm)
    }

    pub fn call_3(
        &self,
        idx: usize,
        arg1: Value,
        arg2: Value,
        arg3: Value,
        vm: &VM,
    ) -> LispResult<Value> {
        self.fns_3[idx](arg1, arg2, arg3, vm)
    }

    pub fn call_n(&self, idx: usize, args: &mut [Value], vm: &VM) -> LispResult<Value> {
        self.fns_n[idx](args, vm)
    }

    pub fn get_(&self, key: Symbol) -> Option<&(LispFnType, u16, Arity)> {
        self.mapping.get(&key)
    }
}

pub fn load(reg: &mut BuiltinRegistry) {
    list::load(reg);
    math::load(reg);
    bitwise::load(reg);
    misc::load(reg);
    string::load(reg);
    types::load(reg);
    comparison::load(reg);
    bignum::load(reg);
}
