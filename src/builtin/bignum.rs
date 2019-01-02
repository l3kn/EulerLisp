use num::BigInt;

use crate::{Datum, LispResult};
use crate::builtin::*;
use crate::vm::VM;

fn number_to_bignum(n: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bignum(BigInt::from(n.as_integer()?)))
}

fn bignum_from_digits(digits: Datum, _vm: &VM) -> LispResult {
    let digits = digits.as_pair()?.collect_list()?;
    let mut pow = BigInt::from(1);
    let mut result = BigInt::from(0);

    for digit in digits {
        result += digit.as_integer()? * &pow;
        pow *= 10;
    }

    Ok(Datum::Bignum(result))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("bignum", number_to_bignum);
    reg.register1("digits->bignum", bignum_from_digits);
}
