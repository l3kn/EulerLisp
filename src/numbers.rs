use std::ops::Add;
use std::ops::Mul;
use std::ops::Sub;
use std::ops::Neg;
use std::ops::Div;

use ::Datum;

use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rational {
    pub num: isize,
    pub denom: isize // Not needed but makes calculations easier
}

impl Rational {
    pub fn new(num: isize, denom: isize) -> Self {
        if denom == 0 {
            panic!("denominator must be != 0");
        }

        let gcd = gcd(num, denom);

        let num = num / gcd;
        let denom = denom / gcd;

        if denom < 0 {
            Self { num: -num, denom: -denom }
        } else {
            Self { num: num, denom: denom }
        }

    }

    pub fn reduce(&self) -> Datum {
        if self.denom == 1 {
            Datum::Integer(self.num)
        } else {
            Datum::Rational(self.clone())
        }
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.num, self.denom)
    }
}

impl Add for Rational {
    type Output = Rational;

    fn add(self, other: Rational) -> Rational {
        let num = self.num * other.denom + other.num * self.denom;
        let denom = self.denom * other.denom;
        Rational::new(num, denom)
    }
}

impl Add<isize> for Rational {
    type Output = Rational;

    fn add(self, other: isize) -> Rational {
        let num = self.num + other * self.denom;
        let denom = self.denom;
        Rational::new(num, denom)
    }
}

impl Add<Rational> for isize {
    type Output = Rational;

    fn add(self, other: Rational) -> Rational {
        let num = other.num + self * other.denom;
        let denom = other.denom;
        Rational::new(num, denom)
    }
}

impl Sub for Rational {
    type Output = Rational;

    fn sub(self, other: Rational) -> Rational {
        let num = self.num * other.denom - other.num * self.denom;
        let denom = self.denom * other.denom;
        Rational::new(num, denom)
    }
}

impl Sub<isize> for Rational {
    type Output = Rational;

    fn sub(self, other: isize) -> Rational {
        let num = self.num - other * self.denom;
        let denom = self.denom;
        Rational::new(num, denom)
    }
}

impl Sub<Rational> for isize {
    type Output = Rational;

    fn sub(self, other: Rational) -> Rational {
        let num = self * other.denom - other.num;
        let denom = other.denom;
        Rational::new(num, denom)
    }
}

impl Neg for Rational {
    type Output = Rational;

    fn neg(self) -> Rational {
        Rational::new(-self.num, self.denom)
    }
}

impl Mul for Rational {
    type Output = Rational;

    fn mul(self, other: Rational) -> Rational {
        Rational::new(self.num * other.num, self.denom * other.denom)
    }
}

impl Mul<isize> for Rational {
    type Output = Rational;

    fn mul(self, other: isize) -> Rational {
        Rational::new(self.num * other, self.denom)
    }
}

impl Mul<Rational> for isize {
    type Output = Rational;

    fn mul(self, other: Rational) -> Rational {
        Rational::new(self * other.num, other.denom)
    }
}

impl Div for Rational {
    type Output = Rational;

    fn div(self, other: Rational) -> Rational {
        Rational::new(self.num * other.denom, self.denom * other.num)
    }
}

impl Div<isize> for Rational {
    type Output = Rational;

    fn div(self, other: isize) -> Rational {
        Rational::new(self.num, self.denom * other)
    }
}

impl Div<Rational> for isize {
    type Output = Rational;

    fn div(self, other: Rational) -> Rational {
        Rational::new(self * other.denom, other.num)
    }
}

fn gcd(x: isize, y: isize) -> isize {
    let mut x = x;
    let mut y = y;
    while y != 0 {
        let t = y;
        y = x % y;
        x = t;
    }
    x
}
