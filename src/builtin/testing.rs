#![allow(clippy::needless_pass_by_value)]

use crate::{Datum, LispResult};
use crate::builtin::*;
use crate::vm::OutputRef;
use crate::symbol_table::SymbolTable;
use crate::heap::Heap;

fn make_cons(a1: Datum, a2: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let (pair_ref, pair) = heap.allocate_pair();
    pair.0 = a1;
    pair.1 = a2;

    Ok(Datum::Pair_(pair_ref))
}

fn fst(a1: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    match a1 {
        Datum::Pair_(pair_ref) => {
            let d = heap.get_pair(pair_ref);
            Ok(d.0.clone())
        },
        _ => panic!("fst_ of non pair_")
    }
}

fn rst(a1: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    match a1 {
        Datum::Pair_(pair_ref) => {
            let d = heap.get_pair(pair_ref);
            Ok(d.1.clone())
        },
        _ => panic!("rst_ of non pair_")
    }
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register2("cons_", make_cons);
    reg.register1("fst_", fst);
    reg.register1("rst_", rst);
}
