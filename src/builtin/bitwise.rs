#![allow(clippy::needless_pass_by_value)]

use std::convert::TryInto;

use crate::builtin::*;
use crate::vm::VM;
use crate::{Arity, Datum, LispResult};

fn bin_bitwise_and(a: Datum, b: Datum, _vm: &VM) -> LispResult<Datum> {
    let a: isize = a.try_into()?;
    let b: isize = b.try_into()?;
    Ok(Datum::Integer(a & b))
}

fn bitwise_and(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
    let mut res = vs[0].take().try_into()?;
    for v in &mut vs[1..] {
        let v: isize = v.take().try_into()?;
        res &= v;
    }
    Ok(Datum::Integer(res))
}

fn bin_bitwise_or(a: Datum, b: Datum, _vm: &VM) -> LispResult<Datum> {
    let a: isize = a.try_into()?;
    let b: isize = b.try_into()?;
    Ok(Datum::Integer(a | b))
}

fn bitwise_or(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
    let mut res = vs[0].take().try_into()?;
    for v in &mut vs[1..] {
        let v: isize = v.take().try_into()?;
        res |= v;
    }
    Ok(Datum::Integer(res))
}

fn bin_bitwise_xor(a: Datum, b: Datum, _vm: &VM) -> LispResult<Datum> {
    let a: isize = a.try_into()?;
    let b: isize = b.try_into()?;
    Ok(Datum::Integer(a ^ b))
}

fn bitwise_xor(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
    let mut res = vs[0].take().try_into()?;
    for v in &mut vs[1..] {
        let v: isize = v.take().try_into()?;
        res ^= v;
    }
    Ok(Datum::Integer(res))
}

fn bitwise_not(v: Datum, _vm: &VM) -> LispResult<Datum> {
    let res: isize = v.try_into()?;
    Ok(Datum::Integer(!res))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register2("__binbitwise-and", bin_bitwise_and);
    reg.register_var("__varbitwise-and", bitwise_and, Arity::Min(2));
    reg.register2("__binbitwise-or", bin_bitwise_or);
    reg.register_var("__varbitwise-or", bitwise_or, Arity::Min(2));
    reg.register2("__binbitwise-xor", bin_bitwise_xor);
    reg.register_var("__varbitwise-xor", bitwise_xor, Arity::Min(2));
    reg.register1("bitwise-not", bitwise_not);
}
