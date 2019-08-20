use crate::{Arity, LispFn1, LispFn2, LispFn3, LispFnN};

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
    pub fns_1: Vec<(String, LispFn1)>,
    pub fns_2: Vec<(String, LispFn2)>,
    pub fns_3: Vec<(String, LispFn3)>,
    pub fns_n: Vec<(String, LispFnN, Arity)>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        Self {
            fns_1: Vec::new(),
            fns_2: Vec::new(),
            fns_3: Vec::new(),
            fns_n: Vec::new(),
        }
    }

    pub fn register_var(&mut self, name: &str, f: LispFnN, arity: Arity) {
        self.fns_n.push((name.to_string(), f, arity));
    }

    pub fn register1(&mut self, name: &str, f: LispFn1) {
        self.fns_1.push((name.to_string(), f));
    }

    pub fn register2(&mut self, name: &str, f: LispFn2) {
        self.fns_2.push((name.to_string(), f));
    }

    pub fn register3(&mut self, name: &str, f: LispFn3) {
        self.fns_3.push((name.to_string(), f));
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
