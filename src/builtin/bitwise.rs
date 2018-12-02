use builtin::*;
use Arity;
use Datum;
use LispResult;

use vm::VM;

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

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register2("__binbitwise-and", bin_bitwise_and);
    reg.register_var("__varbitwise-and", bitwise_and, Arity::Min(2));
    reg.register2("__binbitwise-or", bin_bitwise_or);
    reg.register_var("__varbitwise-or", bitwise_or, Arity::Min(2));
    reg.register2("__binbitwise-xor", bin_bitwise_xor);
    reg.register_var("__varbitwise-xor", bitwise_xor, Arity::Min(2));
    reg.register1("bitwise-not", bitwise_not);
}
