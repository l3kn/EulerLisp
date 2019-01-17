#![allow(clippy::needless_pass_by_value)]

use std::convert::TryInto;

use num::BigInt;

use crate::builtin::*;
use crate::vm::VM;
use crate::{Value, LispResult};

fn number_to_bignum(n: Value, _vm: &VM) -> LispResult<Value> {
    let n: isize = n.try_into()?;
    Ok(Value::Bignum(BigInt::from(n)))
}

fn bignum_from_digits(digits: Value, _vm: &VM) -> LispResult<Value> {
    let digits = digits.as_pair()?.collect_list()?;
    let mut pow = BigInt::from(1);
    let mut result = BigInt::from(0);

    for digit in digits {
        let digit: isize = digit.try_into()?;
        result += digit * &pow;
        pow *= 10;
    }

    Ok(Value::Bignum(result))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("bignum", number_to_bignum);
    reg.register1("digits->bignum", bignum_from_digits);
}
