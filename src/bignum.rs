use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;
use std::ops::Rem;
use std::fmt;

// Parts of the implementation are basend on TAoCP Vol. 2, Ch. 4.3.1
// TODO: Implement Bignum - Bignum division
// TODO: Fix addition / subtraction implemnentation, handle signs better

use std::cmp::{PartialOrd, Ordering};

use ::IntegerDiv;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Bignum {
    pub sign: bool,
    pub data: Vec<usize>
}

impl PartialOrd for Bignum {
    fn partial_cmp(&self, other: &Bignum) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Bignum {
    fn cmp(&self, other: &Bignum) -> Ordering {
        match (self.sign, other.sign) {
            (true, true) => {
                let len = self.data.len();
                let len_other = other.data.len();

                if len > len_other {
                    Ordering::Less
                } else if len < len_other {
                    Ordering::Greater
                } else {
                    for j in 0..len {
                        let res = other.data[len - j - 1].cmp(&self.data[len - j - 1]);
                        if res != Ordering::Equal {
                            return res;
                        }
                    }
                    Ordering::Equal
                }
            },
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            (false, false) => {
                let len = self.data.len();
                let len_other = other.data.len();
                
                if len > len_other {
                    Ordering::Greater
                } else if len < len_other {
                    Ordering::Less
                } else {
                    for j in 0..len {
                        let res = self.data[len - j - 1].cmp(&other.data[len - j - 1]);
                        if res != Ordering::Equal {
                            return res;
                        }
                    }
                    Ordering::Equal
                }
            }
        }
    }
}

// 10^9, this way the product of the sum of two parts still fits inside a u64
pub const BASE: usize = 1000000000;
pub const DIGITS: usize = 9;

impl Bignum {
    pub fn new(value: isize) -> Self {
        if value == 0 {
            return Self { sign: false, data: Vec::new() }
        }

        let sign;
        let mut uvalue; 
        if value < 0 {
            sign = true;
            uvalue = (-value) as usize;
        } else {
            sign = false;
            uvalue = value as usize;
        }

        let mut data = Vec::new();
        while uvalue > 0 {
            data.push(uvalue % BASE);
            uvalue /= BASE;
        }

        Self {
            sign: sign,
            data: data
        }
    }

    fn from_data(sign: bool, mut data: Vec<usize>) -> Self {
        // Remove leading zeros
        while data.len() > 1 && data[data.len() - 1] == 0 {
            data.pop();
        }
        Self { sign, data }
    }

    pub fn from_chunks(chunks: Vec<usize>) -> Self {
        return Self { sign: false, data: chunks }
    }

    pub fn to_isize(&self) -> isize {
        if self.data.len() > 1 {
            panic!("Bignum is to big for isize");
        } else {
            if self.sign {
                -(*self.data.get(0).unwrap_or(&0) as isize)
            } else {
                (*self.data.get(0).unwrap_or(&0) as isize)
            }
        }
    }

    pub fn num_digits(&self) -> isize {
        let len = self.data.len();

        if len == 0 {
            1
        } else {
            let last = *self.data.get(len - 1).unwrap() as f64;
            (((len - 1) * 9) as isize) + (last.log(10.0) as isize) + 1
        }
    }

    pub fn digits(&self) -> Vec<isize> {
        if self.data.len() == 0 {
            vec![0]
        } else {
            let mut result = Vec::new();
            for (i, chunk) in self.data.iter().enumerate() {
                let mut remaining = DIGITS;
                let mut cur = *chunk;
                while cur > 0{
                    result.push((cur % 10) as isize);
                    cur /= 10;
                    remaining -= 1;
                }

                if i < (self.data.len() - 1) {
                    while remaining > 0 {
                        result.push(0);
                        remaining -= 1;
                    }
                }
            }
            result
        }
    }

    pub fn chunks(&self) -> Vec<isize> {
        self.data.iter().map(|x| *x as isize).collect()
    }

    ///// Divide a bignum by another bignum,
    ///// producing a quotient-remainder tuple
    /////
    ///// Based on Algorithm D, TAoCP Vol. 2, Section 4.3.1
    //fn div_mod(&self, other: &Bignum) -> (Bignum, Bignum) {
    //    // Normalize

    //    // Make sure the number is at least as large as the divisor
    //    if vector_cmp(&self.data, &other.data) == Ordering::Less {
    //        return (Bignum::new(0), self.clone());
    //    }

    //    let n = other.data.len();
    //    let m = self.data.len() - n;

    //    let mut d = BASE / other.data[n - 1];

    //    let mut u = vector_mul_i(&self.data, d);
    //    let mut v = vector_mul_i(&other.data, d);

    //    // Initialize
    //    let mut j = m;

    //    // Calculate q_hat
    //    let mut q_hat = (u[j + n] * BASE + u[j + n - 1]) / v[n-1];
    //    let mut r_hat = (u[j + n] * BASE + u[j + n - 1]) % v[n-1];

    //    if q_hat >= BASE || q_hat * v[n - 2] > BASE * r_hat + u[j + n - 2] {
    //        q_hat -= 1;
    //        r_hat += v[n - 1];
    //    }

    //    if r_hat < BASE {
    //        if q_hat >= BASE || q_hat * v[n - 2] > BASE * r_hat + u[j + n - 2] {
    //            q_hat -= 1;
    //            r_hat += v[n - 1];
    //        }
    //    }

    //    // Multiply and subtract
    //    u = vector_sub(&u, &vector_mul_i(&v, q_hat));

    //    // TODO: Complete this implementation


    //    (Bignum::new(0), Bignum::new(0))
    //}
}

/// Addition for positive numbers
///
/// Based on Algorithm A, TAoCP Vol. 2, Section 4.3.1
fn vector_add(a: &Vec<usize>, b: &Vec<usize>) -> Vec<usize> {
    let n = a.len().max(b.len());
    let mut result = Vec::new();
    let mut carry = 0;

    for j in 0..n {
        let u = *a.get(j).unwrap_or(&0);
        let v = *b.get(j).unwrap_or(&0);

        let res = u + v + carry;
        result.push(res % BASE);
        carry = res / BASE;
    }

    if carry > 0 {
        result.push(carry);
    }

    result
}

// Subtraction for positive numbers,
// a - b, assuming a >= b
//
// Based on Algorithm S, Section 4.3.1, TAoCP Vol. 2
// TODO: The handling of negative results is strange,
// is there a better way to do this?
fn vector_sub(a: &Vec<usize>, b: &Vec<usize>) -> Vec<usize> {
    let n = a.len();
    let mut result = Vec::new();
    let mut carry = 0_isize;

    for j in 0..n {
        let u = *a.get(j).unwrap();
        let v = *b.get(j).unwrap_or(&0);

        let res : isize = (u as isize) - (v as isize) - carry;

        if res < 0 {
            carry = 1;
        } else {
            carry = 0
        }

        let res_u = if res < 0 {
           (res + (BASE as isize)) as usize
        } else {
            res as usize
        };

        result.push(res_u % BASE)
    }

    // If this is the case, the assumption a >= b was wrong
    if carry == 1 {
        panic!("Invalid bignum subtraction, carry was -1 at the end");
    }

    // Remove leading 0s
    for j in 0..n {
        if result[n - j - 1] == 0 {
            result.pop();
        } else {
            break
        }
    }

    result
}

// Naive multiplication
// TODO: Implement Karatsuba (if possible) or FFT multiplication
fn vector_mul(a: &Vec<usize>, b: &Vec<usize>) -> Vec<usize> {
    let mut result = Vec::new();

    for (i, item) in a.iter().enumerate() {
        let mut chunk_result = Vec::new();
        let mut chunk_carry = 0;
        for chunk in b {
            let res = (chunk * item) + chunk_carry;
            chunk_result.push(res % BASE);
            chunk_carry = res / BASE;
        }

        // TODO: Is this loop necessary?
        while chunk_carry > 0 {
            chunk_result.push(chunk_carry % BASE);
            chunk_carry /= BASE;
        }

        for _ in 0..i {
            chunk_result.insert(0, 0);
        }
        result = vector_add(&result, &chunk_result);
    }

    result
}

fn vector_mul_i(a: &Vec<usize>, b: usize) -> Vec<usize> {
    let mut result = Vec::new();

    for (i, item) in a.iter().enumerate() {
        let mut chunk_result = Vec::new();
        let mut chunk_carry = 0;

        let res = (b * item) + chunk_carry;
        chunk_result.push(res % BASE);
        chunk_carry = res / BASE;

        // TODO: Is this loop necessary?
        while chunk_carry > 0 {
            chunk_result.push(chunk_carry % BASE);
            chunk_carry /= BASE;
        }

        for _ in 0..i {
            chunk_result.insert(0, 0);
        }
        result = vector_add(&result, &chunk_result);
    }

    result
}

fn vector_cmp(a: &Vec<usize>, b: &Vec<usize>) -> Ordering {
    let len = a.len();
    let len_other = b.len();

    if len > len_other {
        Ordering::Greater
    } else if len < len_other {
        Ordering::Less
    } else {
        for j in 0..len {
            let res = a[len - j - 1].cmp(&b[len - j - 1]);
            if res != Ordering::Equal {
                return res;
            }
        }
        Ordering::Equal
    }
}

// TODO: Downcast to integer if small enough
impl Add for Bignum {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        if self.sign {
            if other.sign {
                let res = vector_add(&self.data, &other.data);
                Self {
                    sign: true,
                    data: res,
                }
            } else {
                other + self
            }
        } else {
            let ord = vector_cmp(&self.data, &other.data);

            if other.sign {
                // Relation of a to b
                match ord {
                    Ordering::Less => {
                        let res = vector_sub(&other.data, &self.data);
                        Self::from_data(true, res)
                    },
                    Ordering::Equal => {
                        Self {
                            sign: false,
                            data: vec![0]
                        }
                    },
                    Ordering::Greater => {
                        let res = vector_sub(&self.data, &other.data);
                        Self::from_data(false, res)
                    },
                }
            } else {
                let res = vector_add(&self.data, &other.data);
                Self {
                    sign: false,
                    data: res,
                }
            }
        }
    }
}

impl Sub for Bignum {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        if self.sign || other.sign {
            panic!("Subtraction of Bignums < 0 is not implemented yet");
        }
        let new_data = vector_sub(&self.data, &other.data);

        Self::from_data(false, new_data)
    }
}

impl Mul for Bignum {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let new_data = vector_mul(&self.data, &other.data);

        Self {
            sign: (self.sign ^ other.sign),
            data: new_data,
        }
    }
}

// See TAoCP Vol. 2, Ch. 4.3.1, Ex. 16
impl Rem<isize> for Bignum {
    type Output = isize;

    fn rem(self, other: isize) -> isize {
        let v = other.abs() as usize;
        if v > BASE {
            panic!("Bignum dividend to big");
        }

        let mut w = vec![0; self.data.len()];
        let mut r = 0;

        for j in (0..self.data.len()).rev() {
            w[j] = (r * BASE + self.data[j]) / v;
            r = (r * BASE + self.data[j]) % v;
        }

        r as isize
    }
}

// See TAoCP Vol. 2, Ch. 4.3.1, Ex. 16
impl IntegerDiv<isize> for Bignum {
    type Output = Self;

    fn int_div(self, other: isize) -> Self {
        let v = other.abs() as usize;
        if v > BASE {
            panic!("Bignum dividend to big");
        }

        let mut w = vec![0; self.data.len()];
        let mut r = 0;

        for j in (0..self.data.len()).rev() {
            w[j] = (r * BASE + self.data[j]) / v;
            r = (r * BASE + self.data[j]) % v;
        }

        Self::from_data(self.sign ^ (other < 0), w)
    }
}


impl fmt::Display for Bignum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.sign {
            write!(f, "-")?;
        }

        if self.data.len() == 0 {
            write!(f, "0")?;
        } else {
            let mut iter = self.data.iter().rev();
            write!(f, "{}", iter.nth(0).unwrap())?;
            for datum in iter {
                write!(f, "{:09}", datum)?;
            }
        }
        write!(f, "")
    }
}
