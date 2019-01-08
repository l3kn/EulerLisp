#![allow(clippy::needless_pass_by_value)]

use std::cmp::Ordering;

use rand::{thread_rng, Rng};

use crate::{Arity, Datum, LispErr, LispResult};
use crate::builtin::*;
use crate::LispErr::*;
use crate::vm::OutputRef;
use crate::symbol_table::SymbolTable;
use crate::heap::Heap;

fn cons(fst: Datum, rst: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {

    Ok(heap.make_pair(fst, rst))
}

fn fst(pair: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    if let Datum::Pair(pair_ref) = pair {
        let pair = heap.get_pair(pair_ref);
        Ok(pair.0)
    } else {
        Err(LispErr::TypeError("convert", "pair", pair))
    }
}

fn rst(pair: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    if let Datum::Pair(pair_ref) = pair {
        let pair = heap.get_pair(pair_ref);
        Ok(pair.1)
    } else {
        Err(LispErr::TypeError("convert", "pair", pair))
    }
}

fn set_fst(pair: Datum, fst: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    if let Datum::Pair(pair_ref) = pair {
        heap.get_pair_mut(pair_ref).0 = fst;
        Ok(Datum::Undefined)
    } else {
        Err(LispErr::TypeError("convert", "pair", pair))
    }
}

fn set_rst(pair: Datum, rst: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    if let Datum::Pair(pair_ref) = pair {
        heap.get_pair_mut(pair_ref).1 = rst;
        Ok(Datum::Undefined)
    } else {
        Err(LispErr::TypeError("convert", "pair", pair))
    }
}

fn list(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    Ok(heap.make_list_from_vec(vs.to_vec()))
}

fn vector(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    Ok(heap.make_vector(vs.to_vec()))
}

fn make_vector(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let len = vs[0].as_uinteger()?;
    let default = if vs.len() == 2 {
        vs[1]
    } else {
        Datum::Undefined
    };
    let vector = vec![default; len as usize];
    Ok(heap.make_vector(vector))
}

fn sort(list: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    match list {
        Datum::Pair(ptr) => {
            let mut elems = heap.get_pair_list(ptr)?;
            let mut es = elems.as_mut_slice();
            let len = es.len();
            quicksort_helper(&mut es, 0, (len - 1) as isize, heap)?;
            Ok(heap.make_list_from_vec(es.to_vec()))
        }
        Datum::Nil => Ok(Datum::Nil),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn quicksort_helper(arr: &mut [Datum], left: isize, right: isize, heap: &mut Heap) -> Result<bool, LispErr> {
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
            while (&arr[i as usize]).compare(&*val, heap).unwrap() == Ordering::Less {
                i += 1
            }
            j -= 1;
            while (&*val).compare(&arr[j as usize], heap).unwrap() == Ordering::Less {
                if j == left {
                    break;
                }
                j -= 1;
            }
            if i >= j {
                break;
            }
            arr.swap(i as usize, j as usize);
            if (&arr[i as usize]).compare(&*val, heap).unwrap() == Ordering::Equal {
                p += 1;
                arr.swap(p as usize, i as usize)
            }
            if (&*val).compare(&arr[j as usize], heap).unwrap() == Ordering::Equal {
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

    quicksort_helper(arr, left, j, heap)?;
    quicksort_helper(arr, i, right, heap)?;

    Ok(true)
}

// Heap's algorithm
fn permutations(list: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let mut elems = 
        match list {
            Datum::Pair(ptr) => heap.get_pair_list(ptr)?,
            Datum::Nil => vec![],
            _ => return Err(InvalidTypeOfArguments),
        };
    let mut result: Vec<Datum> = Vec::new();

    let n = elems.len();
    let mut c = vec![0; n];

    result.push(heap.make_list_from_vec(elems.clone()));
    let mut i = 0;
    while i < n {
        if c[i] < i {
            if i % 2 == 0 {
                elems.swap(0, i);
            } else {
                elems.swap(c[i], i);
            }
            result.push(heap.make_list_from_vec(elems.clone()));
            c[i] += 1;
            i = 0;
        } else {
            c[i] = 0;
            i += 1;
        }
    }

    Ok(heap.make_list_from_vec(result))
}

fn combinations(len: Datum, list: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let len = len.as_uinteger()?;
    let elems = 
        match list {
            Datum::Pair(ptr) => heap.get_pair_list(ptr)?,
            Datum::Nil => vec![],
            _ => return Err(InvalidTypeOfArguments),
        };

    let max = elems.len();
    let mut counters = vec![0; len];
    let mut result: Vec<Datum> = Vec::new();
    let mut done = false;

    while !done {
        let cur: Vec<Datum> = counters.iter().map(|c| elems[*c]).collect();
        result.push(heap.make_list_from_vec(cur));

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

    Ok(heap.make_list_from_vec(result))
}

fn uniq(list: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    match list {
        Datum::Pair(ptr) => {
            let mut elems = heap.get_pair_list(ptr)?;
            elems.dedup();
            Ok(heap.make_list_from_vec(elems))
        }
        Datum::Nil => Ok(Datum::Nil),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn join(joiner: Datum, list: Datum, _out: &OutputRef, st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let joiner = match joiner {
        Datum::String(ptr) => heap.get_string(ptr),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    match list {
        Datum::Pair(ptr) => {
            let parts = heap.get_pair_list(ptr)?;
            let mut res = String::new();

            for i in 0..parts.len() {
                res += &(parts[i].to_string(st, heap, false));
                if i < (parts.len() - 1) {
                    res += &joiner;
                }
            }

            Ok(heap.make_string(res))
        }
        Datum::Nil => Ok(heap.make_string(String::new())),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn vector_ref(vector: Datum, index: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    match vector.get(index.as_uinteger()?) {
        Some(e) => Ok(*e),
        None => Err(IndexOutOfBounds),
    }
}

fn vector_set(vector: Datum, index: Datum, val: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector_mut(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    let index = index.as_uinteger()?;
    if index < vector.len() {
        vector[index] = val;
        Ok(Datum::Undefined)
    } else {
        Err(IndexOutOfBounds)
    }
}

fn vector_push(vector: Datum, val: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector_mut(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    vector.push(val);
    Ok(Datum::Undefined)
}

fn vector_pop(vector: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector_mut(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    Ok(vector.pop().unwrap_or(Datum::Undefined))
}

// Fisher-Yates Shuffle
// SEE: TAOCP, Volume 2, Third Edition: Algorithm P, Shuffling (page 142)
fn vector_shuffle(vector: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector_mut(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    let mut rng = thread_rng();
    for j in (0..vector.len()).rev() {
        let k = rng.gen_range(0, j + 1);
        vector.swap(j, k);
    }

    Ok(Datum::Undefined)
}

fn vector_delete(vector: Datum, index: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector_mut(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    let index = index.as_uinteger()?;
    vector.remove(index);

    Ok(Datum::Undefined)
}

fn vector_length(vector: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    Ok(Datum::Integer(vector.len() as isize))
}

fn vector_copy(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vs[0] {
            heap.get_vector(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };

    let from =
        if vs.len() > 1 {
            vs[1].as_uinteger()?
        } else {
            0
        };

    let to =
        if vs.len() > 2 {
            vs[2].as_uinteger()?
        } else {
            vector.len()
        };

    let new: Vec<Datum> = vector.iter().skip(from).take(to - from).cloned().collect();
    Ok(heap.make_vector(new))
}

fn list_to_vector(list: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    match list {
        Datum::Pair(ptr) => {
            let elems = heap.get_pair_list(ptr)?;
            Ok(heap.make_vector(elems))
        }
        Datum::Nil => Ok(heap.make_vector(vec![])),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn vector_to_list(vector: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let vector =
        if let Datum::Vector(ptr) = vector {
            heap.get_vector(ptr)
        } else {
            return Err(LispErr::InvalidTypeOfArguments);
        };
    Ok(heap.make_list_from_vec(vector.clone()))
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
