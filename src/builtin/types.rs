use std::collections::HashMap;

use ::Datum;
use LispFn;
use ::LispResult;

use builtin::*;
use compiler::vm::VM;

fn pair_questionmark(v: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bool(v.is_pair()))
}

fn nil_questionmark(v: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bool(v.is_nil()))
}

fn integer_questionmark(v: Datum, _vm: &VM) -> LispResult {
    if let Datum::Integer(_) = v {
        Ok(Datum::Bool(true))
    } else {
        Ok(Datum::Bool(false))
    }
}

fn rational_questionmark(v: Datum, _vm: &VM) -> LispResult {
    if let Datum::Rational(_) = v {
        Ok(Datum::Bool(true))
    } else {
        Ok(Datum::Bool(false))
    }
}

fn bignum_questionmark(v: Datum, _vm: &VM) -> LispResult {
    if let Datum::Bignum(_) = v {
        Ok(Datum::Bool(true))
    } else {
        Ok(Datum::Bool(false))
    }
}

pub fn load(hm: &mut HashMap<String, LispFn>) {
    register1(hm, "pair?", pair_questionmark);
    register1(hm, "nil?", nil_questionmark);
    register1(hm, "integer?", integer_questionmark);
    register1(hm, "rational?", rational_questionmark);
    register1(hm, "bignum?", bignum_questionmark);
}
