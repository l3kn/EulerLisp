#![allow(clippy::needless_pass_by_value)]

use std::cmp::Ordering;
use std::convert::TryInto;

use rand::{thread_rng, Rng};

use crate::builtin::*;
use crate::vm::VM;
use crate::LispErr::*;
use crate::{Arity, Datum, LispErr, LispResult};

fn cons(fst: Datum, rst: Datum, _vm: &VM) -> LispResult<Datum> {
    Ok(Datum::make_pair(fst, rst))
}

fn fst(pair: Datum, _vm: &VM) -> LispResult<Datum> {
    Ok(pair.as_pair()?.0.clone())
}

fn rst(pair: Datum, _vm: &VM) -> LispResult<Datum> {
    Ok(pair.as_pair()?.1.clone())
}

fn set_fst(pair: Datum, fst: Datum, _vm: &VM) -> LispResult<Datum> {
    pair.as_mut_pair()?.0 = fst;
    Ok(Datum::Undefined)
}

fn set_rst(pair: Datum, rst: Datum, _vm: &VM) -> LispResult<Datum> {
    pair.as_mut_pair()?.1 = rst;
    Ok(Datum::Undefined)
}

fn list(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
    Ok(Datum::make_list(vs))
}

fn vector(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
    Ok(Datum::make_vector(vs))
}

fn make_vector(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
    let len: usize = vs[0].take().try_into()?;
    let default = if vs.len() == 2 {
        vs[1].clone()
    } else {
        Datum::Undefined
    };
    let vector = vec![default; len];
    Ok(Datum::make_vector_from_vec(vector))
}

fn sort(list: Datum, _vm: &VM) -> LispResult<Datum> {
    match list {
        Datum::Pair(ptr) => {
            let mut elems = ptr.borrow().collect_list()?;
            let mut es = elems.as_mut_slice();
            let len = es.len();
            quicksort_helper(&mut es, 0, (len - 1) as isize)?;
            Ok(Datum::make_list(es))
        }
        Datum::Nil => Ok(Datum::Nil),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn quicksort_helper(arr: &mut [Datum], left: isize, right: isize) -> Result<bool, LispErr> {
    if right <= left {
        return Ok(true);
    }

    let mut i: isize = left - 1;
    let mut j: isize = right;
    let mut p: isize = i;
    let mut q: isize = j;
    unsafe {
        let val: *mut Datum = &mut arr[right as usize];
        loop {
            i += 1;
            while (&arr[i as usize]).compare(&*val).unwrap() == Ordering::Less {
                i += 1
            }
            j -= 1;
            while (&*val).compare(&arr[j as usize]).unwrap() == Ordering::Less {
                if j == left {
                    break;
                }
                j -= 1;
            }
            if i >= j {
                break;
            }
            arr.swap(i as usize, j as usize);
            if (&arr[i as usize]).compare(&*val).unwrap() == Ordering::Equal {
                p += 1;
                arr.swap(p as usize, i as usize)
            }
            if (&*val).compare(&arr[j as usize]).unwrap() == Ordering::Equal {
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
fn permutations(list: Datum, _vm: &VM) -> LispResult<Datum> {
    let mut elems = list.as_pair()?.collect_list()?;
    let mut result: Vec<Datum> = Vec::new();

    let n = elems.len();
    let mut c = vec![0; n];

    result.push(Datum::make_list_from_vec(elems.clone()));
    let mut i = 0;
    while i < n {
        if c[i] < i {
            if i % 2 == 0 {
                elems.swap(0, i);
            } else {
                elems.swap(c[i], i);
            }
            result.push(Datum::make_list_from_vec(elems.clone()));
            c[i] += 1;
            i = 0;
        } else {
            c[i] = 0;
            i += 1;
        }
    }

    Ok(Datum::make_list_from_vec(result))
}

fn combinations(len: Datum, list: Datum, _vm: &VM) -> LispResult<Datum> {
    let len = len.try_into()?;
    let elems = list.as_pair()?.collect_list()?;

    let max = elems.len();
    let mut counters = vec![0; len];
    let mut result: Vec<Datum> = Vec::new();
    let mut done = false;

    while !done {
        let cur: Vec<Datum> = counters.iter().map(|c| elems[*c].clone()).collect();
        result.push(Datum::make_list_from_vec(cur));

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

    Ok(Datum::make_list_from_vec(result))
}

fn uniq(list: Datum, _vm: &VM) -> LispResult<Datum> {
    match list {
        Datum::Pair(ptr) => {
            let mut elems = ptr.borrow().collect_list()?;
            elems.dedup();
            Ok(Datum::make_list_from_vec(elems))
        }
        Datum::Nil => Ok(Datum::Nil),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn join(joiner: Datum, list: Datum, vm: &VM) -> LispResult<Datum> {
    let joiner: String = joiner.try_into()?;
    let pair = list.as_pair()?;
    let parts = pair.collect_list()?;

    let mut res = String::new();
    for i in 0..parts.len() {
        if let Datum::String(ref s) = parts[i] {
            res += s;
        } else {
            res += &(parts[i].to_string(&vm.symbol_table.borrow()));
        }
        if i < (parts.len() - 1) {
            res += &joiner;
        }
    }

    Ok(Datum::String(res))
}

fn vector_ref(vector: Datum, index: Datum, _vm: &VM) -> LispResult<Datum> {
    let vector = vector.as_vector()?;
    let index: usize = index.try_into()?;
    match vector.get(index) {
        Some(e) => Ok(e.clone()),
        None => Err(IndexOutOfBounds),
    }
}

fn vector_set(vector: Datum, index: Datum, val: Datum, _vm: &VM) -> LispResult<Datum> {
    let mut vector = vector.as_mut_vector()?;
    let index: usize = index.try_into()?;
    if index < vector.len() {
        vector[index] = val;
        Ok(Datum::Undefined)
    } else {
        Err(IndexOutOfBounds)
    }
}

fn vector_push(vector: Datum, val: Datum, _vm: &VM) -> LispResult<Datum> {
    let mut vector = vector.as_mut_vector()?;
    vector.push(val);
    Ok(Datum::Undefined)
}

fn vector_pop(vector: Datum, _vm: &VM) -> LispResult<Datum> {
    let mut vector = vector.as_mut_vector()?;
    Ok(vector.pop().unwrap_or(Datum::Undefined))
}

// Fisher-Yates Shuffle
// SEE: TAOCP, Volume 2, Third Edition: Algorithm P, Shuffling (page 142)
fn vector_shuffle(vector: Datum, _vm: &VM) -> LispResult<Datum> {
    let mut vector = vector.as_mut_vector()?;
    let mut rng = thread_rng();
    for j in (0..vector.len()).rev() {
        let k = rng.gen_range(0, j + 1);
        vector.swap(j, k);
    }

    Ok(Datum::Undefined)
}

fn vector_delete(vector: Datum, index: Datum, _vm: &VM) -> LispResult<Datum> {
    let mut vector = vector.as_mut_vector()?;
    let index = index.try_into()?;
    vector.remove(index);

    Ok(Datum::Undefined)
}

fn vector_length(vector: Datum, _vm: &VM) -> LispResult<Datum> {
    let vector = vector.as_vector()?;
    Ok(Datum::Integer(vector.len() as isize))
}

fn vector_copy(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
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

    let new: Vec<Datum> = vector.iter().skip(from).take(to - from).cloned().collect();
    Ok(Datum::make_vector_from_vec(new))
}

fn list_to_vector(list: Datum, _vm: &VM) -> LispResult<Datum> {
    if list.is_nil() {
        Ok(Datum::make_vector_from_vec(vec![]))
    } else {
        let pair = list.as_pair()?;
        let elems = pair.collect_list()?;
        Ok(Datum::make_vector_from_vec(elems))
    }
}

fn vector_to_list(vector: Datum, _vm: &VM) -> LispResult<Datum> {
    let vector = vector.as_vector()?;
    Ok(Datum::make_list_from_vec(vector.clone()))
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
