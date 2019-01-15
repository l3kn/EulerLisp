#![allow(clippy::needless_pass_by_value)]

use std::convert::TryInto;

use num::BigInt;

use crate::builtin::*;
use crate::vm::VM;
use crate::{Datum, LispResult};

fn number_to_bignum(n: Datum, _vm: &VM) -> LispResult<Datum> {
    let n: isize = n.try_into()?;
    Ok(Datum::Bignum(BigInt::from(n)))
}

fn bignum_from_digits(digits: Datum, _vm: &VM) -> LispResult<Datum> {
    let digits = digits.as_pair()?.collect_list()?;
    let mut pow = BigInt::from(1);
    let mut result = BigInt::from(0);

    for digit in digits {
        let digit: isize = digit.try_into()?;
        result += digit * &pow;
        pow *= 10;
    }

    Ok(Datum::Bignum(result))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("bignum", number_to_bignum);
    reg.register1("digits->bignum", bignum_from_digits);
}
