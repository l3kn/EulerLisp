#![allow(clippy::needless_pass_by_value)]

use std::convert::TryInto;

use crate::builtin::*;
use crate::vm::VM;
use crate::{Arity, LispResult, Value};

fn bin_bitwise_and(a: Value, b: Value, _vm: &VM) -> LispResult<Value> {
    let a: isize = a.try_into()?;
    let b: isize = b.try_into()?;
    Ok(Value::Integer(a & b))
}

fn bitwise_and(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let mut res = vs[0].take().try_into()?;
    for v in &mut vs[1..] {
        let v: isize = v.take().try_into()?;
        res &= v;
    }
    Ok(Value::Integer(res))
}

fn bin_bitwise_or(a: Value, b: Value, _vm: &VM) -> LispResult<Value> {
    let a: isize = a.try_into()?;
    let b: isize = b.try_into()?;
    Ok(Value::Integer(a | b))
}

fn bitwise_or(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let mut res = vs[0].take().try_into()?;
    for v in &mut vs[1..] {
        let v: isize = v.take().try_into()?;
        res |= v;
    }
    Ok(Value::Integer(res))
}

fn bin_bitwise_xor(a: Value, b: Value, _vm: &VM) -> LispResult<Value> {
    let a: isize = a.try_into()?;
    let b: isize = b.try_into()?;
    Ok(Value::Integer(a ^ b))
}

fn bitwise_xor(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let mut res = vs[0].take().try_into()?;
    for v in &mut vs[1..] {
        let v: isize = v.take().try_into()?;
        res ^= v;
    }
    Ok(Value::Integer(res))
}

fn bitwise_not(v: Value, _vm: &VM) -> LispResult<Value> {
    let res: isize = v.try_into()?;
    Ok(Value::Integer(!res))
}

pub fn load(reg: &mut dyn BuiltinRegistry) {
    reg.register2("__binbitwise-and", bin_bitwise_and);
    reg.register_var("__varbitwise-and", bitwise_and, Arity::Min(2));
    reg.register2("__binbitwise-or", bin_bitwise_or);
    reg.register_var("__varbitwise-or", bitwise_or, Arity::Min(2));
    reg.register2("__binbitwise-xor", bin_bitwise_xor);
    reg.register_var("__varbitwise-xor", bitwise_xor, Arity::Min(2));
    reg.register1("bitwise-not", bitwise_not);
}
