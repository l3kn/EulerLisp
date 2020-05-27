#![allow(clippy::needless_pass_by_value)]

use crate::builtin::*;
use crate::vm::VM;
use crate::{LispResult, Value};

fn pair_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    Ok(Value::Bool(v.is_pair()))
}

fn nil_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    Ok(Value::Bool(v.is_nil()))
}

fn integer_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::Integer(_) = v {
        Ok(Value::Bool(true))
    } else {
        Ok(Value::Bool(false))
    }
}

fn float_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::Float(_) = v {
        Ok(Value::Bool(true))
    } else {
        Ok(Value::Bool(false))
    }
}

fn rational_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::Rational(_) = v {
        Ok(Value::Bool(true))
    } else {
        Ok(Value::Bool(false))
    }
}

fn bignum_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::Bignum(_) = v {
        Ok(Value::Bool(true))
    } else {
        Ok(Value::Bool(false))
    }
}

fn string_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::String(_) = v {
        Ok(Value::Bool(true))
    } else {
        Ok(Value::Bool(false))
    }
}

fn bool_questionmark(v: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::Bool(_) = v {
        Ok(Value::Bool(true))
    } else {
        Ok(Value::Bool(false))
    }
}

pub fn load(reg: &mut dyn BuiltinRegistry) {
    reg.register1("pair?", pair_questionmark);
    reg.register1("nil?", nil_questionmark);
    reg.register1("integer?", integer_questionmark);
    reg.register1("float?", float_questionmark);
    reg.register1("string?", string_questionmark);
    reg.register1("rational?", rational_questionmark);
    reg.register1("bignum?", bignum_questionmark);
    reg.register1("bool?", bool_questionmark);
}
