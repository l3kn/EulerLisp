use std::cmp::Ordering;

use Arity;
use Datum;
use LispResult;

use builtin::*;
use vm::VM;

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

fn is_equal(vs: &mut [Datum], _vm: &VM) -> LispResult {
    for i in 0..(vs.len() - 1) {
        if !vs[i].is_equal(&vs[i + 1])? {
            return Ok(Datum::Bool(false));
        }
    }
    Ok(Datum::Bool(true))
}

fn eq(vs: &mut [Datum], _vm: &VM) -> LispResult {
    for i in 0..(vs.len() - 1) {
        if vs[i].compare(&vs[i + 1])? != Ordering::Equal {
            return Ok(Datum::Bool(false));
        }
    }
    Ok(Datum::Bool(true))
}

fn neq(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bool(a != b))
}

fn lt(vs: &mut [Datum], _vm: &VM) -> LispResult {
    for i in 0..(vs.len() - 1) {
        if vs[i].compare(&vs[i + 1])? != Ordering::Less {
            return Ok(Datum::Bool(false));
        }
    }
    Ok(Datum::Bool(true))
}

fn gt(vs: &mut [Datum], _vm: &VM) -> LispResult {
    for i in 0..(vs.len() - 1) {
        if vs[i].compare(&vs[i + 1])? != Ordering::Greater {
            return Ok(Datum::Bool(false));
        }
    }
    Ok(Datum::Bool(true))
}

fn lte(vs: &mut [Datum], _vm: &VM) -> LispResult {
    for i in 0..(vs.len() - 1) {
        if vs[i + 1].compare(&vs[i])? == Ordering::Less {
            return Ok(Datum::Bool(false));
        }
    }
    Ok(Datum::Bool(true))
}

fn gte(vs: &mut [Datum], _vm: &VM) -> LispResult {
    for i in 0..(vs.len() - 1) {
        if vs[i + 1].compare(&vs[i])? == Ordering::Greater {
            return Ok(Datum::Bool(false));
        }
    }
    Ok(Datum::Bool(true))
}

fn max(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut max = vs[0].clone();
    for v in vs.into_iter().skip(1) {
        if max.compare(v)? == Ordering::Less {
            max = v.clone();
        }
    }
    Ok(max)
}

fn bin_max(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    if a.compare(&b)? == Ordering::Less {
        Ok(b)
    } else {
        Ok(a)
    }
}

fn min(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut min = vs[0].clone();
    for v in vs.into_iter().skip(1) {
        if min.compare(v)? == Ordering::Greater {
            min = v.clone();
        }
    }
    Ok(min)
}

fn bin_min(a: Datum, b: Datum, _vm: &VM) -> LispResult {
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
