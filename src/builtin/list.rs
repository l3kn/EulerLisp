#![allow(clippy::needless_pass_by_value)]

use std::cmp::Ordering;
use std::convert::TryInto;

use rand::{thread_rng, Rng};

use crate::builtin::*;
use crate::vm::VM;
use crate::LispError::*;
use crate::{Arity, LispResult, Value};

fn cons(fst: Value, rst: Value, _vm: &VM) -> LispResult<Value> {
    Ok(Value::make_pair(fst, rst))
}

fn fst(pair: Value, _vm: &VM) -> LispResult<Value> {
    Ok(pair.as_pair()?.get_fst().clone())
}

fn rst(pair: Value, _vm: &VM) -> LispResult<Value> {
    Ok(pair.as_pair()?.get_rst().clone())
}

fn set_fst(pair: Value, fst: Value, _vm: &VM) -> LispResult<Value> {
    pair.as_pair()?.set_fst(fst);
    Ok(Value::Undefined)
}

fn set_rst(pair: Value, rst: Value, _vm: &VM) -> LispResult<Value> {
    pair.as_pair()?.set_rst(rst);
    Ok(Value::Undefined)
}

fn list(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    Ok(Value::make_list(vs))
}

fn vector(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    Ok(Value::make_vector(vs))
}

fn make_vector(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let len: usize = vs[0].take().try_into()?;
    let default = if vs.len() == 2 {
        vs[1].clone()
    } else {
        Value::Undefined
    };
    let vector = vec![default; len];
    Ok(Value::make_vector_from_vec(vector))
}

fn sort(list: Value, _vm: &VM) -> LispResult<Value> {
    match list {
        Value::Pair(ptr) => {
            let mut elems = ptr.collect_list()?;
            let mut es = elems.as_mut_slice();
            let len = es.len();
            quicksort_helper(&mut es, 0, (len - 1) as isize)?;
            Ok(Value::make_list(es))
        }
        Value::Nil => Ok(Value::Nil),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn quicksort_helper(arr: &mut [Value], left: isize, right: isize) -> LispResult<bool> {
    if right <= left {
        return Ok(true);
    }

    let mut i: isize = left - 1;
    let mut j: isize = right;
    let mut p: isize = i;
    let mut q: isize = j;
    unsafe {
        let val: *mut Value = &mut arr[right as usize];
        loop {
            i += 1;
            while (&arr[i as usize]).compare(&*val)? == Ordering::Less {
                i += 1
            }
            j -= 1;
            while (&*val).compare(&arr[j as usize])? == Ordering::Less {
                if j == left {
                    break;
                }
                j -= 1;
            }
            if i >= j {
                break;
            }
            arr.swap(i as usize, j as usize);
            if (&arr[i as usize]).compare(&*val)? == Ordering::Equal {
                p += 1;
                arr.swap(p as usize, i as usize)
            }
            if (&*val).compare(&arr[j as usize])? == Ordering::Equal {
                q -= 1;
                arr.swap(j as usize, q as usize)
            }
        }
    }

    arr.swap(i as usize, right as usize);
    j = i - 1;
    i += 1;
    let mut k: isize = left;
    while k < p {
        arr.swap(k as usize, j as usize);
        k += 1;
        j -= 1;
        assert!(k < arr.len() as isize);
    }
    k = right - 1;
    while k > q {
        arr.swap(i as usize, k as usize);
        k -= 1;
        i += 1;
        assert!(k != 0);
    }

    quicksort_helper(arr, left, j)?;
    quicksort_helper(arr, i, right)?;

    Ok(true)
}

// Heap's algorithm
fn permutations(list: Value, _vm: &VM) -> LispResult<Value> {
    let mut elems = list.as_list()?;
    let mut result: Vec<Value> = Vec::new();

    let n = elems.len();
    let mut c = vec![0; n];

    result.push(Value::make_list_from_vec(elems.clone()));
    let mut i = 0;
    while i < n {
        if c[i] < i {
            if i % 2 == 0 {
                elems.swap(0, i);
            } else {
                elems.swap(c[i], i);
            }
            result.push(Value::make_list_from_vec(elems.clone()));
            c[i] += 1;
            i = 0;
        } else {
            c[i] = 0;
            i += 1;
        }
    }

    Ok(Value::make_list_from_vec(result))
}

fn combinations(len: Value, list: Value, _vm: &VM) -> LispResult<Value> {
    let len = len.try_into()?;
    let elems = list.as_list()?;

    let max = elems.len();
    let mut counters = vec![0; len];
    let mut result: Vec<Value> = Vec::new();
    let mut done = false;

    while !done {
        let cur: Vec<Value> = counters.iter().map(|c| elems[*c].clone()).collect();
        result.push(Value::make_list_from_vec(cur));

        for i in 0..len {
            let new = counters[i] + 1;
            if new >= max {
                counters[i] = 0;
                if i == (len - 1) {
                    done = true;
                }
            } else {
                counters[i] = new;
                break;
            }
        }
    }

    Ok(Value::make_list_from_vec(result))
}

fn uniq(list: Value, _vm: &VM) -> LispResult<Value> {
    match list {
        Value::Pair(ptr) => {
            let mut elems = ptr.collect_list()?;
            elems.dedup();
            Ok(Value::make_list_from_vec(elems))
        }
        Value::Nil => Ok(Value::Nil),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn join(joiner: Value, list: Value, vm: &VM) -> LispResult<Value> {
    let joiner: String = joiner.try_into()?;
    let parts = list.as_list()?;

    let mut res = String::new();
    for i in 0..parts.len() {
        if let Value::String(ref s) = parts[i] {
            res += s;
        } else {
            res += &(parts[i].to_string());
        }
        if i < (parts.len() - 1) {
            res += &joiner;
        }
    }

    Ok(Value::String(res))
}

fn vector_ref(vector: Value, index: Value, _vm: &VM) -> LispResult<Value> {
    let vector = vector.as_vector()?;
    let index: usize = index.try_into()?;
    match vector.get(index) {
        Some(e) => Ok(e.clone()),
        None => Err(IndexOutOfBounds),
    }
}

fn vector_set(vector: Value, index: Value, val: Value, _vm: &VM) -> LispResult<Value> {
    let mut vector = vector.as_mut_vector()?;
    let index: usize = index.try_into()?;
    if index < vector.len() {
        vector[index] = val;
        Ok(Value::Undefined)
    } else {
        Err(IndexOutOfBounds)
    }
}

fn vector_push(vector: Value, val: Value, _vm: &VM) -> LispResult<Value> {
    let mut vector = vector.as_mut_vector()?;
    vector.push(val);
    Ok(Value::Undefined)
}

fn vector_pop(vector: Value, _vm: &VM) -> LispResult<Value> {
    let mut vector = vector.as_mut_vector()?;
    Ok(vector.pop().unwrap_or(Value::Undefined))
}

// Fisher-Yates Shuffle
// SEE: TAOCP, Volume 2, Third Edition: Algorithm P, Shuffling (page 142)
fn vector_shuffle(vector: Value, _vm: &VM) -> LispResult<Value> {
    let mut vector = vector.as_mut_vector()?;
    let mut rng = thread_rng();
    for j in (0..vector.len()).rev() {
        let k = rng.gen_range(0, j + 1);
        vector.swap(j, k);
    }

    Ok(Value::Undefined)
}

fn vector_delete(vector: Value, index: Value, _vm: &VM) -> LispResult<Value> {
    let mut vector = vector.as_mut_vector()?;
    let index = index.try_into()?;
    vector.remove(index);

    Ok(Value::Undefined)
}

fn vector_length(vector: Value, _vm: &VM) -> LispResult<Value> {
    let vector = vector.as_vector()?;
    Ok(Value::Integer(vector.len() as isize))
}

fn vector_copy(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let vector = vs[0].take();
    let vector = vector.as_vector()?;

    let from = if vs.len() > 1 {
        vs[1].take().try_into()?
    } else {
        0
    };

    let to = if vs.len() > 2 {
        vs[2].take().try_into()?
    } else {
        vector.len()
    };

    let new: Vec<Value> = vector.iter().skip(from).take(to - from).cloned().collect();
    Ok(Value::make_vector_from_vec(new))
}

fn list_to_vector(list: Value, _vm: &VM) -> LispResult<Value> {
    if list.is_nil() {
        Ok(Value::make_vector_from_vec(vec![]))
    } else {
        let elems = list.as_list()?;
        Ok(Value::make_vector_from_vec(elems))
    }
}

fn vector_to_list(vector: Value, _vm: &VM) -> LispResult<Value> {
    let vector = vector.as_vector()?;
    Ok(Value::make_list_from_vec(vector.clone()))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register2("cons", cons);
    reg.register1("fst", fst);
    reg.register1("rst", rst);
    reg.register2("set-fst!", set_fst);
    reg.register2("set-rst!", set_rst);
    reg.register2("vector-ref", vector_ref);
    reg.register3("vector-set!", vector_set);
    reg.register2("vector-push!", vector_push);
    reg.register1("vector-pop!", vector_pop);
    reg.register1("vector-shuffle!", vector_shuffle);
    reg.register2("vector-delete!", vector_delete);
    reg.register1("vector-length", vector_length);
    reg.register1("list->vector", list_to_vector);
    reg.register1("vector->list", vector_to_list);
    reg.register1("sort", sort);
    reg.register1("permutations", permutations);
    reg.register2("combinations", combinations);
    reg.register1("uniq", uniq);
    reg.register2("join", join);

    reg.register_var("vector-copy", vector_copy, Arity::Range(1, 3));
    reg.register_var("list", list, Arity::Min(0));
    reg.register_var("vector", vector, Arity::Min(0));
    reg.register_var("make-vector", make_vector, Arity::Range(1, 2));
}
