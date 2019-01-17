#![allow(clippy::needless_pass_by_value)]

use std::cmp::Ordering;

use crate::{Arity, Value, LispResult};
use crate::builtin::*;
use crate::vm::VM;

// Scheme has four kinds of equality:
//
// 1. `=` check for numeric equality
// 2. `eq?` checks for pointer equality
// 3. `eqv?` is a superset of `eq?`
//    and returns true for equal primitive vaules
// 4. `equal?` is the same as `eqv?`,
//    but extended to compare lists and vectors
//
// Right now only `=` and `equal?` are implemented
// and `=` supports all kinds of primitive values,
// not only numbers.

fn is_equal(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    for i in 0..(vs.len() - 1) {
        if !vs[i].is_equal(&vs[i + 1])? {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn eq(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    for i in 0..(vs.len() - 1) {
        if vs[i].compare(&vs[i + 1])? != Ordering::Equal {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn neq(a: Value, b: Value, _vm: &VM) -> LispResult<Value> {
    Ok(Value::Bool(a != b))
}

fn lt(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    for i in 0..(vs.len() - 1) {
        if vs[i].compare(&vs[i + 1])? != Ordering::Less {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn gt(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    for i in 0..(vs.len() - 1) {
        if vs[i].compare(&vs[i + 1])? != Ordering::Greater {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn lte(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    for i in 0..(vs.len() - 1) {
        if vs[i + 1].compare(&vs[i])? == Ordering::Less {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn gte(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    for i in 0..(vs.len() - 1) {
        if vs[i + 1].compare(&vs[i])? == Ordering::Greater {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn max(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let mut max = vs[0].clone();
    for v in vs.into_iter().skip(1) {
        if max.compare(v)? == Ordering::Less {
            max = v.clone();
        }
    }
    Ok(max)
}

fn bin_max(a: Value, b: Value, _vm: &VM) -> LispResult<Value> {
    if a.compare(&b)? == Ordering::Less {
        Ok(b)
    } else {
        Ok(a)
    }
}

fn min(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let mut min = vs[0].clone();
    for v in vs.into_iter().skip(1) {
        if min.compare(v)? == Ordering::Greater {
            min = v.clone();
        }
    }
    Ok(min)
}

fn bin_min(a: Value, b: Value, _vm: &VM) -> LispResult<Value> {
    if a.compare(&b)? == Ordering::Greater {
        Ok(b)
    } else {
        Ok(a)
    }
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register2("!=", neq);
    reg.register_var("__varequal?", is_equal, Arity::Min(2));
    reg.register_var("__var=", eq, Arity::Min(2));
    reg.register_var("__var<", lt, Arity::Min(2));
    reg.register_var("__var>", gt, Arity::Min(2));
    reg.register_var("__var<=", lte, Arity::Min(2));
    reg.register_var("__var>=", gte, Arity::Min(2));
    reg.register_var("__varmax", max, Arity::Min(2));
    reg.register_var("__varmin", min, Arity::Min(2));
    reg.register2("__binmax", bin_max);
    reg.register2("__binmin", bin_min);
}
