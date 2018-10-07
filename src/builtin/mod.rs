use std::collections::HashMap;

use ::Datum;
use ::LispFn1;
use ::LispFn2;
use ::LispFn3;
use ::LispFnN;
use ::LispFnType;
use ::LispResult;
use ::Arity;

use compiler::vm::VM;

mod list;
mod primes;
mod math;
mod bitwise;
mod misc;
mod types;
mod comparison;
mod string;
mod bignum;
mod data_structures;

// The difference between builtins and special forms is
// that special forms choose if they want to eval their arguments themselves,
// builtins are called with evaluated arguments

#[derive(Clone)]
pub struct BuiltinRegistry {
    mapping: HashMap<String, (LispFnType, u32, Arity)>,
    fns_1: Vec<LispFn1>,
    fns_2: Vec<LispFn2>,
    fns_3: Vec<LispFn3>,
    fns_n: Vec<LispFnN>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        BuiltinRegistry {
            mapping: HashMap::new(),
            fns_1: Vec::new(),
            fns_2: Vec::new(),
            fns_3: Vec::new(),
            fns_n: Vec::new(),
        }
    }

    pub fn register_var(&mut self, name: &str, f: LispFnN, arity: Arity) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Variadic, self.fns_n.len() as u32, arity)
        );
        self.fns_n.push(f);
    }

    pub fn register1(&mut self, name: &str, f: LispFn1) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed1, self.fns_1.len() as u32, Arity::Exact(1))
        );
        self.fns_1.push(f);
    }

    pub fn register2(&mut self, name: &str, f: LispFn2) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed2, self.fns_2.len() as u32, Arity::Exact(2))
        );
        self.fns_2.push(f);
    }

    pub fn register3(&mut self, name: &str, f: LispFn3) {
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed3, self.fns_3.len() as u32, Arity::Exact(3))
        );
        self.fns_3.push(f);
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.mapping.contains_key(key)
    }

    pub fn call_1(&self, f: u32, arg: Datum, vm: &VM) -> LispResult {
        self.fns_1[f as usize](arg, vm)
    }

    pub fn call_2(&self, f: u32, arg1: Datum, arg2: Datum, vm: &VM) -> LispResult {
        self.fns_2[f as usize](arg1, arg2, vm)
    }

    pub fn call_3(&self, f: u32, arg1: Datum, arg2: Datum, arg3: Datum, vm: &VM) -> LispResult {
        self.fns_3[f as usize](arg1, arg2, arg3, vm)
    }

    pub fn call_n(&self, f: u32, args: &mut [Datum], vm: &VM) -> LispResult {
        self.fns_n[f as usize](args, vm)
    }

    pub fn get_(&self, key: &str) -> Option<&(LispFnType, u32, Arity)> {
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
    data_structures::load(reg);
}
