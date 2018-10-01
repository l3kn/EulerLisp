use std::collections::HashMap;

use ::Datum;
use ::LispFn;
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

pub fn register_var(
    hm: &mut HashMap<String, LispFn>,
    name: &str,
    f: fn(&mut [Datum], &VM) -> LispResult,
    arity: Arity,
) {
    hm.insert(name.to_string(), LispFn::Variadic(f, arity));
}

pub fn register1(
    hm: &mut HashMap<String, LispFn>,
    name: &str,
    f: fn(Datum, &VM) -> LispResult,
) {
    hm.insert(name.to_string(), LispFn::Fixed1(f));
}

pub fn register2(
    hm: &mut HashMap<String, LispFn>,
    name: &str,
    f: fn(Datum, Datum, &VM) -> LispResult,
) {
    hm.insert(name.to_string(), LispFn::Fixed2(f));
}

pub fn register3(
    hm: &mut HashMap<String, LispFn>,
    name: &str,
    f: fn(Datum, Datum, Datum, &VM) -> LispResult,
) {
    hm.insert(name.to_string(), LispFn::Fixed3(f));
}

pub fn load(hm: &mut HashMap<String, LispFn>) {
    list::load(hm);
    math::load(hm);
    bitwise::load(hm);
    misc::load(hm);
    string::load(hm);
    types::load(hm);
    comparison::load(hm);
    bignum::load(hm);
    data_structures::load(hm);
}
