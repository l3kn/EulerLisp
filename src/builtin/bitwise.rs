#![allow(clippy::needless_pass_by_value)]

use crate::{Arity, Datum, LispResult};
use crate::builtin::*;
use crate::vm::OutputRef;
use crate::symbol_table::SymbolTable;
use crate::heap::Heap;

fn bin_bitwise_and(a: Datum, b: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a & b))
}

fn bitwise_and(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res &= v.as_integer()?;
    }
    Ok(Datum::Integer(res))
}

fn bin_bitwise_or(a: Datum, b: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a | b))
}

fn bitwise_or(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res |= v.as_integer()?;
    }
    Ok(Datum::Integer(res))
}

fn bin_bitwise_xor(a: Datum, b: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a ^ b))
}

fn bitwise_xor(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res ^= v.as_integer()?;
    }
    Ok(Datum::Integer(res))
}

fn bitwise_not(v: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
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
