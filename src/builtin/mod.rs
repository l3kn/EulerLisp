use std::collections::HashMap;

use ::Datum;
use ::LispFn;
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
    base_mapping: HashMap<String, LispFn>,
    // TODO: Replace i32 with enum
    mapping: HashMap<String, (LispFnType, u32, Arity)>,
    fns1: Vec<LispFn1>,
    fns2: Vec<LispFn2>,
    fns3: Vec<LispFn3>,
    fnsN: Vec<LispFnN>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        BuiltinRegistry {
            base_mapping: HashMap::new(),
            mapping: HashMap::new(),
            fns1: Vec::new(),
            fns2: Vec::new(),
            fns3: Vec::new(),
            fnsN: Vec::new(),
        }
    }

    pub fn register_var(&mut self, name: &str, f: LispFnN, arity: Arity) {
        self.base_mapping.insert(
            name.to_string(),
            LispFn::Variadic(f, arity.clone())
        );
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Variadic, self.fnsN.len() as u32, arity)
        );
        self.fnsN.push(f);
    }

    pub fn register1(&mut self, name: &str, f: LispFn1) {
        self.base_mapping.insert(name.to_string(), LispFn::Fixed1(f));
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed1, self.fns1.len() as u32, Arity::Exact(1))
        );
        self.fns1.push(f);
    }

    pub fn register2(&mut self, name: &str, f: LispFn2) {
        self.base_mapping.insert(name.to_string(), LispFn::Fixed2(f));
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed2, self.fns2.len() as u32, Arity::Exact(2))
        );
        self.fns2.push(f);
    }

    pub fn register3(&mut self, name: &str, f: LispFn3) {
        self.base_mapping.insert(name.to_string(), LispFn::Fixed3(f));
        self.mapping.insert(
            name.to_string(),
            (LispFnType::Fixed3, self.fns3.len() as u32, Arity::Exact(3))
        );
        self.fns3.push(f);
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.base_mapping.contains_key(key)
    }

    pub fn call1(&self, f: u32, arg: Datum, vm: &VM) -> LispResult {
        self.fns1[f as usize](arg, vm)
    }

    pub fn call2(&self, f: u32, arg1: Datum, arg2: Datum, vm: &VM) -> LispResult {
        self.fns2[f as usize](arg1, arg2, vm)
    }

    pub fn call3(&self, f: u32, arg1: Datum, arg2: Datum, arg3: Datum, vm: &VM) -> LispResult {
        self.fns3[f as usize](arg1, arg2, arg3, vm)
    }

    pub fn callN(&self, f: u32, args: &mut [Datum], vm: &VM) -> LispResult {
        self.fnsN[f as usize](args, vm)
    }

    pub fn get(&self, key: &str) -> Option<&LispFn> {
        self.base_mapping.get(key)
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
