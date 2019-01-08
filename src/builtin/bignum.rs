#![allow(clippy::needless_pass_by_value)]

use num::BigInt;

use crate::{Datum, LispResult, LispErr};
use crate::builtin::*;
use crate::vm::OutputRef;
use crate::symbol_table::SymbolTable;
use crate::heap::Heap;

fn number_to_bignum(n: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    Ok(heap.make_bignum(BigInt::from(n.as_integer()?)))
}

fn bignum_from_digits(digits: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let mut pow = BigInt::from(1);
    let mut result = BigInt::from(0);

    match digits {
        Datum::Pair(ptr) => {
            let digits = heap.get_pair_list(ptr)?;
            for digit in digits {
                result += digit.as_integer()? * &pow;
                pow *= 10;
            }
        }
        Datum::Nil => {}
        _ => return Err(LispErr::InvalidTypeOfArguments),
    }

    Ok(heap.make_bignum(result))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("bignum", number_to_bignum);
    reg.register1("digits->bignum", bignum_from_digits);
}
