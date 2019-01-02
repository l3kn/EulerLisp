use std::collections::HashMap;

use crate::{Arity, Datum, LispResult};
use crate::{LispFn1, LispFn2, LispFn3, LispFnN, LispFnType};
use crate::vm::VM;

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

#[derive(Clone)]
pub struct BuiltinRegistry {
    mapping: HashMap<String, (LispFnType, u16, Arity)>,
    // Used for prettyprinting of call instructions
    inverse_mapping: HashMap<(LispFnType, u16), String>,
    fns_1: Vec<LispFn1>,
    fns_2: Vec<LispFn2>,
    fns_3: Vec<LispFn3>,
    fns_n: Vec<LispFnN>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        BuiltinRegistry {
            mapping: HashMap::new(),
            inverse_mapping: HashMap::new(),
            fns_1: Vec::new(),
            fns_2: Vec::new(),
            fns_3: Vec::new(),
            fns_n: Vec::new(),
        }
    }

    pub fn register_var(&mut self, name: &str, f: LispFnN, arity: Arity) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Variadic, self.fns_n.len() as u16, arity),
        );
        self.inverse_mapping.insert(
            (LispFnType::Variadic, self.fns_n.len() as u16),
            name.to_string(),
        );
        self.fns_n.push(f);
    }

    pub fn register1(&mut self, name: &str, f: LispFn1) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed1, self.fns_1.len() as u16, Arity::Exact(1)),
        );
        self.inverse_mapping.insert(
            (LispFnType::Fixed1, self.fns_1.len() as u16),
            name.to_string(),
        );
        self.fns_1.push(f);
    }

    pub fn register2(&mut self, name: &str, f: LispFn2) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed2, self.fns_2.len() as u16, Arity::Exact(2)),
        );
        self.inverse_mapping.insert(
            (LispFnType::Fixed2, self.fns_2.len() as u16),
            name.to_string(),
        );
        self.fns_2.push(f);
    }

    pub fn register3(&mut self, name: &str, f: LispFn3) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed3, self.fns_3.len() as u16, Arity::Exact(3)),
        );
        self.inverse_mapping.insert(
            (LispFnType::Fixed3, self.fns_3.len() as u16),
            name.to_string(),
        );
        self.fns_3.push(f);
    }

    pub fn lookup_name(&self, typ: LispFnType, id: u16) -> &str {
        self.inverse_mapping.get(&(typ, id)).unwrap()
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.mapping.contains_key(key)
    }

    pub fn call_1(&self, idx: usize, arg: Datum, vm: &VM) -> LispResult {
        self.fns_1[idx](arg, vm)
    }

    pub fn call_2(&self, idx: usize, arg1: Datum, arg2: Datum, vm: &VM) -> LispResult {
        self.fns_2[idx](arg1, arg2, vm)
    }

    pub fn call_3(&self, idx: usize, arg1: Datum, arg2: Datum, arg3: Datum, vm: &VM) -> LispResult {
        self.fns_3[idx](arg1, arg2, arg3, vm)
    }

    pub fn call_n(&self, idx: usize, args: &mut [Datum], vm: &VM) -> LispResult {
        self.fns_n[idx](args, vm)
    }

    pub fn get_(&self, key: &str) -> Option<&(LispFnType, u16, Arity)> {
        self.mapping.get(key)
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
