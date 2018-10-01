use std::collections::HashMap;
use rand::{thread_rng, Rng};
use std::f64;
use std::rc::Rc;
use std::cell::RefCell;

use LispFn;
use Datum;
use LispErr::*;
use LispResult;
use Arity;
use Pair;

use builtin::primes::PRIMES;
use builtin::*;
use ::IntegerDiv;
use compiler::vm::VM;

fn isqrt(n: isize) -> isize {
    (n as f64).sqrt() as isize
}

fn totient(mut n: isize) -> isize {
    let mut res = n;
    let to = isqrt(n);

    for p in 2..(to+1) {
        if n % p == 0 {
            while n % p == 0 {
                n /= p
            }
            res -= res / p;
        }
    }

    if n > 1 {
        res -= res / n;
    }

    res
}

fn totient_sum(n: isize) -> isize {
    let l = isqrt(n);
    let mut v = vec![0; (l + 1) as usize];
    let floor_nl = n / l;
    let mut big_v = vec![0; (floor_nl + 1) as usize];

    for x in 1..(l + 1) {
        let mut res = (x * (x + 1)) / 2;
        let isqrtx = isqrt(x);

        for g in 2..(isqrtx + 1) {
            res -= v[(x / g) as usize];
        }

        for z in 1..(isqrtx + 1) {
            if z != x / z {
                res -= ((x / z) - (x / (z + 1))) * v[z as usize] 
            }
        }

        v[x as usize] = res;
    }

    for x_ in 1..(floor_nl + 1) {
        let x = floor_nl - x_ + 1;
        let k = n / x;
        let mut res = (k * (k + 1)) / 2;

        let isqrtk = isqrt(k);

        for g in 2..(isqrtk + 1) {
            if (k / g) <= l {
                res -= v[(k / g) as usize];
            } else {
                res -= big_v[(x * g) as usize];
            }
        }

        for z in 1..(isqrtk + 1) {
            if z != (k / z) {
                res -= ((k / z) - (k / (z + 1))) * v[z as usize];
            }
        }

        big_v[x as usize] = res;
    }

    big_v[1]
}

const WITNESSES: [(isize, &[isize]); 11] = [
    (2_047, &[2]),
    (1_373_653, &[2, 3]),
    (9_080_191, &[31, 73]),
    (25_326_001, &[2, 3, 5]),
    (3_215_031_751, &[2, 3, 5, 7]),
    (4_759_123_141, &[2, 7, 61]),
    (1_122_004_669_633, &[2, 13, 23, 1662803]),
    (2_152_302_898_747, &[2, 3, 5, 7, 11]),
    (3_474_749_660_383, &[2, 3, 5, 7, 11, 13]),
    (341_550_071_728_321, &[2, 3, 5, 7, 11, 13, 17]),
    (3_825_123_056_546_413_051, &[2, 3, 5, 7, 11, 13, 17, 19, 23])
];

fn modexp(base: isize, exponent: isize, modulo: isize) -> isize {
    let mut c = 1;

    let mut base = base as i128;
    let mut exponent = exponent as i128;
    let modulo = modulo as i128;

    while exponent != 0 {
        if exponent % 2 == 1 {
            exponent -= 1;
            c = (base * c) % modulo;
        }
        exponent /= 2;
        base = (base * base) % modulo;
    }
    (c as isize)
}

fn factor2(n: isize) -> (isize, isize) {
    let mut d = n;
    let mut r = 0;

    while (d % 2) == 0 {
        d = d >> 1;
        r += 1;
    }

    (r, d)
}

fn det_miller_rabin(n: isize) -> bool {
    if n < 2 {
        return false;
    }

    // Check against some obvious candidates first
    if (n % 2) == 0 { return n == 2; } 
    if (n % 3) == 0 { return n == 3; } 
    if (n % 5) == 0 { return n == 5; } 
    if (n % 7) == 0 { return n == 7; } 
    if (n % 11) == 0 { return n == 11; } 
    if (n % 13) == 0 { return n == 13; } 
    if (n % 17) == 0 { return n == 17; } 
    if (n % 19) == 0 { return n == 19; } 
    if (n % 23) == 0 { return n == 23; } 
    if (n % 29) == 0 { return n == 29; } 

    let (s, d) = factor2(n - 1);
    let &(_, witnesses) = WITNESSES.iter().find(|&&(max, _)| max > n).unwrap();

    let n_ = n as i128;
    'witness: for &a in witnesses.iter() {
        let mut x = modexp(a, d, n) as i128;
        if x == 1 || x == n_ - 1 {
            continue 'witness;
        }
        for _ in 0..s {
            x = (x * x) % n_;
            if x == 1 {
                return false;
            }
            if x == n_ - 1 {
                continue 'witness;
            }
        }
        return false;
    }
    true
}

fn prime_questionmark(n: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bool(det_miller_rabin(n.as_integer()?)))
}

fn zero_questionmark(n: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Bool(n.is_equal(&Datum::Integer(0))?))
}

fn neg(a: Datum, _vm: &VM) -> LispResult {
    Ok(-a)
}

fn add(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].clone();
    for v in &mut vs[1..] {
        res = res + v.clone();
    }
    Ok(res)
}

fn sub(vs: &mut [Datum], _vm: &VM) -> LispResult {
    if vs.len() == 1 {
        Ok(-vs[0].clone())
    } else {
        let mut res = vs[0].clone();
        for v in &mut vs[1..] {
            res = res - v.clone();
        }
        Ok(res)
    }
}

fn mult(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].clone();

    for v in &mut vs[1..] {
        res = res * v.clone();
    }
    Ok(res)
}

fn int_div(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    Ok(a.int_div(b))
}

fn shift_left(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a << b))
}

fn shift_right(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(a >> b))
}

fn popcount(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    Ok(Datum::Integer(a.count_ones() as isize))
}

fn div(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].clone();

    for v in &mut vs[1..] {
        res = res / v.clone();
    }
    Ok(res)
}

fn modulo(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    Ok(a % b)
}

// TODO: Make this work for all integral types (Integer, Bignum)
fn divmod(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::make_pair(
        Datum::Integer(a / b),
        Datum::Integer(a % b),
    ))
}

fn builtin_modexp(a: Datum, e: Datum, m: Datum, _vm: &VM) -> LispResult {
    let b = a.as_integer()?;
    let e = e.as_integer()?;
    let m = m.as_integer()?;
    Ok(Datum::Integer(modexp(b, e, m)))
}

fn rand(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(thread_rng().gen_range(a, b + 1)))
}

fn prime_factors(a: Datum, _vm: &VM) -> LispResult {
    let mut a = a.as_integer()?;
    let mut res = Datum::Nil;

    if a == 1 {
        return Ok(res);
    }

    for i_ in PRIMES.iter() {
        let i = *i_ as isize;

        if a % i == 0 {
            let mut count = 1;
            a /= i;
            while a % i == 0 {
                a /= i;
                count += 1;
            }

            let factor = Datum::make_pair(Datum::Integer(i), Datum::Integer(count));
            let pair = Pair(factor, res);
            res = Datum::Pair(Rc::new(RefCell::new(pair)));
        }
        if (i * i) > a {
            break;
        }
    }

    let mut i = PRIMES[PRIMES.len() - 1];
    let mut double_step = (i % 3) == 1;

    while i * i <= a {
        if a % i == 0 {
            let mut count = 1;
            a /= i;
            while a % i == 0 {
                a /= i;
                count += 1;
            }

            let factor = Datum::make_pair(Datum::Integer(i), Datum::Integer(count));
            let pair = Pair(factor, res);
            res = Datum::Pair(Rc::new(RefCell::new(pair)));
        }

        // Assuming i is >= 5 (congruent to 2 mod 3)
        // the first step of 2 would yield (1 mod 3)
        // and the second (0 mod 3), which can't be a prime factor,
        // so instead, we can make a step of 4, to get back to (2 mod 3)
        if double_step {
            i += 4;
            double_step = false;
        } else {
            i += 2;
            double_step = true;
        }
    }

    // a is prime
    if a != 1 {
        let factor = Datum::make_pair(Datum::Integer(a), Datum::Integer(1));
        let pair = Pair(factor, res);
        res = Datum::Pair(Rc::new(RefCell::new(pair)));
    }

    Ok(res)
}

fn factors(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let mut result = Vec::new();
    let root = (a as f64).sqrt() as isize;

    result.push(Datum::Integer(1));
    if a > 1 {
        result.push(Datum::Integer(a));
    }
    if a > 2 {
        for i in 2..(root + 1) {
            if a % i == 0 {
                result.push(Datum::Integer(i));
                if (a / i) != i {
                    result.push(Datum::Integer(a / i));
                }
            }
        }
    }

    Ok(Datum::make_list_from_vec(result))
}

fn num_prime_factors(a: Datum, _vm: &VM) -> LispResult {
    let mut a = a.as_integer()?;
    let mut res = 0;

    if a == 1 {
        return Ok(Datum::Integer(res));
    }

    for i_ in PRIMES.iter() {
        let i = *i_ as isize;

        if a % i == 0 {
            a /= i;
            while a % i == 0 {
                a /= i;
            }

            res += 1;
        }
        if (i * i) > a {
            break;
        }
    }

    let mut i = PRIMES[PRIMES.len() - 1];
    let mut double_step = (i % 3) == 1;

    while i * i <= a {
        if a % i == 0 {
            a /= i;
            while a % i == 0 {
                a /= i;
            }

            res += 1;
        }

        if double_step {
            i += 4;
            double_step = false;
        } else {
            i += 2;
            double_step = true;
        }
    }

    // a is prime
    if a != 1 {
        res += 1;
    }

    Ok(Datum::Integer(res))
}

fn primes(to: Datum, _vm: &VM) -> LispResult {
    let to = to.as_uinteger()?;

    // TODO: Just loop and generate new primes
    if to > PRIMES.len() {
        panic!("There are only {} precalculated primes", PRIMES.len());
    }

    let primes = PRIMES[0..to].to_vec().iter().map(|p| Datum::Integer(*p)).collect();
    Ok(Datum::make_list_from_vec(primes))
}

fn digits(n: Datum, _vm: &VM) -> LispResult {
    match n {
        Datum::Integer(mut a) => {
            let mut result = Vec::new();

            while a != 0 {
                result.push(Datum::Integer(a % 10));
                a /= 10;
            }

            return Ok(Datum::make_list_from_vec(result))
        },
        Datum::Bignum(ref a) => {
            let digits = a.digits();
            return Ok(Datum::make_list_from_vec(
                digits.into_iter().map(|d| Datum::Integer(d)).collect()
            ));
        },
        _ => Err(InvalidTypeOfArguments)
    }
}

fn num_digits(n: Datum, _vm: &VM) -> LispResult {
    match n {
        Datum::Integer(a) => {
            let res = (a as f64).log10().floor() + 1.0;
            Ok(Datum::Integer(res as isize))
        },
        Datum::Bignum(ref a) => {
            Ok(Datum::Integer(a.num_digits()))
        },
        ref other => Err(TypeError("num-digits", "integer / bignum", other.clone()))
    }
}

fn digits_to_number(digits: Datum, _vm: &VM) -> LispResult {
    let digits = digits.as_pair()?.collect_list()?;
    let mut pow = 1;
    let mut result = 0;

    for digit in digits {
        result += digit.as_integer()? * pow;
        pow *= 10;
    }

    Ok(Datum::Integer(result))
}

fn numerator(n: Datum, _vm: &VM) -> LispResult {
    match n {
        Datum::Integer(n) => Ok(Datum::Integer(n)),
        Datum::Rational(ref r) => Ok(Datum::Integer(r.num)),
        _ => Err(InvalidTypeOfArguments)
    }
}

fn denominator(n: Datum, _vm: &VM) -> LispResult {
    match n {
        Datum::Integer(_) => Ok(Datum::Integer(1)),
        Datum::Rational(ref r) => Ok(Datum::Integer(r.denom)),
        _ => Err(InvalidTypeOfArguments)
    }
}

fn to_float(a: Datum, _vm: &VM) -> LispResult {
    Ok(Datum::Float(a.as_float()?))
}

fn log10(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Float(a.log10()))
}

fn log2(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Float(a.log2()))
}

fn ln(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Float(a.ln()))
}

fn log(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    let b = b.as_float()?;
    Ok(Datum::Float(a.log(b)))
}

fn sqrt(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Float(a.sqrt()))
}

fn cbrt(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Float(a.cbrt()))
}

fn ceil(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Integer(a.ceil() as isize))
}

fn floor(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Integer(a.floor() as isize))
}

fn round(a: Datum, _vm: &VM) -> LispResult {
    let a = a.as_float()?;
    Ok(Datum::Integer(a.round() as isize))
}

/// See: https://lemire.me/blog/2013/12/26/fastest-way-to-compute-the-greatest-common-divisor/
fn gcd_single(mut u : isize, mut v : isize) -> isize {
    if u == 0 {
        return v;
    }
    if v == 0 {
        return u;
    }

    let shift = (u | v).trailing_zeros();
    u >>= u.trailing_zeros();

    while v != 0 {
        v >>= v.trailing_zeros();
        if u > v {
            let t = v;
            v = u;
            u = t;
        }
        v = v - u;
    }

    u << shift
}
fn lcm_single(x : isize, y : isize) -> isize {
    (x * y) / gcd_single(x, y)
}

fn powf(b: Datum, e: Datum, _vm: &VM) -> LispResult {
    let b = b.as_float()?;
    let e = e.as_float()?;
    Ok(Datum::Float(b.powf(e)))
}

// SEE: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
fn mod_inverse(a: Datum, n: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let n = n.as_integer()?;

    let mut t = 0;
    let mut r = n;
    let mut t_new = 1;
    let mut r_new = a;

    while r_new != 0 {
        let quotient = r / r_new;
        let t_ = t_new;
        let r_ = r_new;

        t_new = t - quotient * t_new;
        r_new = r - quotient * r_new;

        t = t_;
        r = r_;
    }

    if r > 1 {
        panic!("not invertible");
    }

    if t < 0 {
        t += n;
    }

    Ok(Datum::Integer(t))
}

// SEE: Extended Euclidian Algorithm, TAoCP Vol. 2,  page 342
fn extended_euclidian(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    // determine u_1, u_2, u_3 so that uu_1 + vu_2 = u_3 = gcd(u, v)
    let mut u1 = 1;
    let mut u2 = 0;
    let mut u3 = a.as_integer()?;

    let mut v1 = 0;
    let mut v2 = 1;
    let mut v3 = b.as_integer()?;

    while v3 != 0 {
        let q = u3 / v3;
        let t1 = u1 - v1 * q;
        let t2 = u2 - v2 * q;
        let t3 = u3 - v3 * q;

        u1 = v1;
        u2 = v2;
        u3 = v3;

        v1 = t1;
        v2 = t2;
        v3 = t3;
    }

    Ok(Datum::make_list_from_vec(vec![
        Datum::Integer(u1),
        Datum::Integer(u2),
        Datum::Integer(u3),
    ]))
}

fn bin_gcd(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(gcd_single(a, b)))
}

fn gcd(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res = gcd_single(res, v.as_integer()?);
    }
    Ok(Datum::Integer(res))
}

fn bin_lcm(a: Datum, b: Datum, _vm: &VM) -> LispResult {
    let a = a.as_integer()?;
    let b = b.as_integer()?;
    Ok(Datum::Integer(lcm_single(a, b)))
}

fn lcm(vs: &mut [Datum], _vm: &VM) -> LispResult {
    let mut res = vs[0].as_integer()?;
    for v in &mut vs[1..] {
        res = lcm_single(res, v.as_integer()?);
    }
    Ok(Datum::Integer(res))
}

fn sin(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v.sin()))
}

fn cos(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v.cos()))
}

fn tan(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v.tan()))
}

fn asin(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v.asin()))
}

fn acos(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v.acos()))
}

fn atan(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v.atan()))
}

fn radiants(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_float()?;
    Ok(Datum::Float(v * (f64::consts::PI / 180.0)))
}

fn totient_(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_integer()?;
    Ok(Datum::Integer(totient(v)))
}

fn totient_sum_(a: Datum, _vm: &VM) -> LispResult {
    let v = a.as_integer()?;
    Ok(Datum::Integer(totient_sum(v)))
}

pub fn load(hm: &mut HashMap<String, LispFn>) {
    register1(hm, "prime?", prime_questionmark);
    register1(hm, "zero?", zero_questionmark);
    register_var(hm, "__var+", add, Arity::Min(2));
    register_var(hm, "__var-", sub, Arity::Min(1));
    register1(hm, "__neg", neg);
    register_var(hm, "__var*", mult, Arity::Min(2));
    register2(hm, "<<", shift_left);
    register2(hm, ">>", shift_right);
    register1(hm, "popcount", popcount);
    register_var(hm, "__var/", div, Arity::Min(2));
    register2(hm, "%", modulo);
    register2(hm, "div", int_div);
    register2(hm, "divmod", divmod);
    register3(hm, "modexp", builtin_modexp);
    register2(hm, "modular-inverse", mod_inverse);
    register2(hm, "rand", rand);
    register1(hm, "factors", factors);
    register1(hm, "prime-factors", prime_factors);
    register1(hm, "num-prime-factors", num_prime_factors);
    register1(hm, "primes", primes);
    register1(hm, "number->digits", digits);
    register1(hm, "number-of-digits", num_digits);
    register1(hm, "digits->number", digits_to_number);
    register1(hm, "numerator", numerator);
    register1(hm, "denominator", denominator);
    register1(hm, "number->float", to_float);
    register1(hm, "log10", log10);
    register1(hm, "log2", log2);
    register1(hm, "ln", ln);
    register2(hm, "log", log);
    register1(hm, "sqrt", sqrt);
    register1(hm, "cbrt", cbrt);
    register1(hm, "ceil", ceil);
    register1(hm, "floor", floor);
    register1(hm, "round", round);
    register_var(hm, "__vargcd", gcd, Arity::Min(2));
    register_var(hm, "__varlcm", lcm, Arity::Min(2));
    register2(hm, "__bingcd", bin_gcd);
    register2(hm, "__binlcm", bin_lcm);
    register2(hm, "extended-euclidian", extended_euclidian);
    register2(hm, "powf", powf);
    register1(hm, "sin", sin);
    register1(hm, "cos", cos);
    register1(hm, "tan", tan);
    register1(hm, "asin", asin);
    register1(hm, "acos", acos);
    register1(hm, "atan", atan);
    register1(hm, "radiants", radiants);
    register1(hm, "totient", totient_);
    register1(hm, "totient-sum", totient_sum_);
}
