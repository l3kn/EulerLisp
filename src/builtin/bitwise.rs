use std::collections::HashMap;

use LispFn;
use Datum;
use LispResult;
use Arity;
use builtin::*;

use compiler::vm::VM;

fn bin_bitwise_and(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a & b))
}

fn bitwise_and(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res = res & v.as_integer()?;
    }
    Ok(Datum::Integer(res))
}

fn bin_bitwise_or(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a | b))
}

fn bitwise_or(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res = res | v.as_integer()?;
    }
    Ok(Datum::Integer(res))
}

fn bin_bitwise_xor(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a ^ b))
}

fn bitwise_xor(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res = res ^ v.as_integer()?;
    }
    Ok(Datum::Integer(res))
}

fn bitwise_not(v: Datum, _vm: &VM) -> LispResult {
    let res = v.as_integer()?;
    Ok(Datum::Integer(!res))
}

pub fn load(hm: &mut HashMap<String, LispFn>) {
    register2(hm, "__binbitwise-and", bin_bitwise_and);
    register_var(hm, "__varbitwise-and", bitwise_and, Arity::Min(2));
    register2(hm, "__binbitwise-or", bin_bitwise_or);
    register_var(hm, "__varbitwise-or", bitwise_or, Arity::Min(2));
    register2(hm, "__binbitwise-xor", bin_bitwise_xor);
    register_var(hm, "__varbitwise-xor", bitwise_xor, Arity::Min(2));
    register1(hm, "bitwise-not", bitwise_not);
}