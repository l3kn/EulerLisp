use ::Datum;
use ::LispErr::*;
use ::LispResult;
use ::LispErr;

use ::bignum;
use ::builtin::*;
use ::bignum::Bignum;
use compiler::vm::VM;

fn number_to_bignum(n: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bignum(Bignum::new(n.as_integer()?)))
}

fn bignum_from_digits(digits: Datum, _vm: &VM) -> LispResult {
    let digits = digits.as_pair()?.collect_list()?;
    let mut chunks = Vec::new();
    let mut pow = 1;
    let mut result = 0;

    for digit in digits {
        result += digit.as_integer()? * pow;
        pow *= 10;

        if pow == (bignum::BASE as isize) {
            pow = 1;
            chunks.push(result as usize);
            result = 0;
        }
    }
    if result != 0 {
        chunks.push(result as usize);
    }

    Ok(Datum::Bignum(Bignum::from_chunks(chunks)))
}

fn bignum_chunks(v: Datum, _vm: &VM) -> LispResult {
    if let Datum::Bignum(a) = v {
        let digits = a.chunks();
        Ok(Datum::make_list_from_vec(
            digits.into_iter().map(|d| Datum::Integer(d)).collect()
        ))
    } else {
        Err(TypeError("bignum-chunks", "bignum", v))
    }
}

fn bignum_from_chunks(chunks: Datum, _vm: &VM) -> LispResult {
    let chunks = chunks.as_pair()?.collect_list()?;
    let result : Result<Vec<usize>, LispErr> = chunks.into_iter().map(|c| c.as_uinteger()).collect();
    Ok(Datum::Bignum(Bignum::from_chunks(result?)))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("bignum", number_to_bignum);
    reg.register1("digits->bignum", bignum_from_digits);
    reg.register1("bignum-chunks", bignum_chunks);
    reg.register1("chunks->bignum", bignum_from_chunks);
}
