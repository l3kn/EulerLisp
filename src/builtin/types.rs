use crate::{Datum, LispResult};
use crate::builtin::*;
use crate::vm::VM;

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

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("pair?", pair_questionmark);
    reg.register1("nil?", nil_questionmark);
    reg.register1("integer?", integer_questionmark);
    reg.register1("rational?", rational_questionmark);
    reg.register1("bignum?", bignum_questionmark);
}
