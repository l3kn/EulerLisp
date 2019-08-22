/// The difference between builtins and special forms is that special
/// forms choose if they want to eval their arguments themselves,
/// builtins are called with evaluated arguments
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

pub trait BuiltinRegistry {
    fn register1(&mut self, name: &str, fun: LispFn1);
    fn register2(&mut self, name: &str, fun: LispFn2);
    fn register3(&mut self, name: &str, fun: LispFn3);
    fn register_var(&mut self, name: &str, fun: LispFnN, arity: Arity);
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
