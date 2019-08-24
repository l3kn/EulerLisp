use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;

use num::{BigInt, Rational};

use crate::env::EnvRef;
use crate::symbol_table::Symbol;
use crate::{Arity, LispError, LispResult};
use crate::{Fsize, Pair, PairRef, Vector, VectorRef};
use crate::{LispFn1, LispFn2, LispFn3, LispFnN};

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Integer(isize),
    Rational(Rational),
    Float(Fsize),
    Bignum(BigInt),
    Char(char),
    String(String),
    Symbol(Symbol),
    Pair(PairRef),
    Vector(VectorRef),
    Builtin1(Symbol, LispFn1),
    Builtin2(Symbol, LispFn2),
    Builtin3(Symbol, LispFn3),
    BuiltinN(Symbol, LispFnN, Arity),
    ActivationFrame(Vec<Value>),
    // TODO: Use struct enum type
    // stack, env_stack, pc_stack
    Continuation(Vec<Value>, Vec<EnvRef>, Vec<usize>),
    Undefined,
    Nil,
    // offset, arity, dotted?, env
    Closure(usize, usize, bool, EnvRef),
}

impl Value {
    pub fn is_true_list(&self) -> bool {
        match self {
            Value::Pair(p) => p.borrow().1.is_true_list_or_nil(),
            _ => false,
        }
    }

    pub fn is_true_list_or_nil(&self) -> bool {
        match self {
            Value::Pair(p) => p.borrow().1.is_true_list_or_nil(),
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn make_list(elems: &mut [Self]) -> Self {
        let mut res = Value::Nil;
        for next in elems.iter_mut().rev() {
            let pair = Pair(next.take(), res);
            res = Value::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    pub fn make_list_from_vec(elems: Vec<Self>) -> Self {
        let mut res = Value::Nil;
        for mut next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Value::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    pub fn make_vector(elems: &mut [Self]) -> Self {
        Value::Vector(Rc::new(RefCell::new(elems.to_vec())))
    }

    pub fn make_vector_from_vec(elems: Vec<Self>) -> Self {
        Value::Vector(Rc::new(RefCell::new(elems)))
    }

    pub fn make_dotted_list_from_vec(elems: Vec<Self>, tail: Self) -> Self {
        let mut res = tail;
        for mut next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Value::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    pub fn make_pair(fst: Self, rst: Self) -> Self {
        let pair = Pair(fst, rst);
        Value::Pair(Rc::new(RefCell::new(pair)))
    }

    pub fn is_pair(&self) -> bool {
        match *self {
            Value::Pair(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        *self == Value::Nil
    }

    pub fn take(&mut self) -> Self {
        mem::replace(self, Value::Undefined)
    }

    pub fn as_symbol(&self) -> LispResult<Symbol> {
        match *self {
            Value::Symbol(s) => Ok(s),
            ref other => Err(LispError::TypeError("convert", "symbol", other.clone())),
        }
    }

    pub fn as_string(&self) -> LispResult<&str> {
        match *self {
            Value::String(ref s) => Ok(s),
            ref other => Err(LispError::TypeError("convert", "string", other.clone())),
        }
    }

    pub fn as_list(&self) -> LispResult<Vec<Value>> {
        match *self {
            Value::Pair(ref s) => {
                if !self.is_true_list() {
                    return Err(LispError::TypeError("convert", "list", self.clone()));
                }

                s.borrow().collect_list()
            }
            Value::Nil => Ok(vec![]),
            ref other => Err(LispError::TypeError("convert", "symbol", other.clone())),
        }
    }

    pub fn as_pair(&self) -> LispResult<Ref<Pair>> {
        match *self {
            Value::Pair(ref ptr) => Ok(ptr.borrow()),
            ref other => Err(LispError::TypeError("convert", "pair", other.clone())),
        }
    }

    pub fn as_mut_pair(&self) -> LispResult<RefMut<Pair>> {
        match *self {
            Value::Pair(ref ptr) => Ok(ptr.borrow_mut()),
            ref other => Err(LispError::TypeError("convert", "pair", other.clone())),
        }
    }

    pub fn as_vector(&self) -> LispResult<Ref<Vector>> {
        match *self {
            Value::Vector(ref ptr) => Ok(ptr.borrow()),
            ref other => Err(LispError::TypeError("convert", "vector", other.clone())),
        }
    }

    pub fn as_mut_vector(&self) -> LispResult<RefMut<Vector>> {
        match *self {
            Value::Vector(ref ptr) => Ok(ptr.borrow_mut()),
            ref other => Err(LispError::TypeError("convert", "vector", other.clone())),
        }
    }

    pub fn is_false(&self) -> bool {
        match *self {
            Value::Nil | Value::Bool(false) => true,
            _ => false,
        }
    }

    pub fn is_true(&self) -> bool {
        match *self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    // TODO: Better error handling
    // TODO: Distinction between `=`, `eq?`, `eqv?` and `equal?`
    pub fn compare(&self, other: &Self) -> LispResult<Ordering> {
        use self::Value::*;
        match (self, other) {
            (&Integer(ref a), &Integer(ref b)) => Ok(a.cmp(b)),
            (&Symbol(ref a), &Symbol(ref b)) => Ok(a.cmp(b)),
            (&Bignum(ref a), &Bignum(ref b)) => Ok(a.cmp(b)),
            (&Integer(a), &Bignum(ref b)) => Ok(BigInt::from(a).cmp(b)),
            (&Bignum(ref a), &Integer(b)) => Ok(a.cmp(&BigInt::from(b))),
            (&Rational(ref a), &Rational(ref b)) => Ok(a.cmp(&b)),
            // TODO: The two below can overflow
            (&Integer(ref a), &Rational(ref b)) => Ok((a * b.denom()).cmp(&(b.numer()))),
            (&Rational(ref a), &Integer(ref b)) => Ok(a.numer().cmp(&(b * a.denom()))),
            (other, &Float(ref b)) => {
                let other: f64 = other.try_into()?;
                Ok(other.partial_cmp(b).unwrap())
            }
            (&Float(ref b), other) => Ok(b.partial_cmp(&(other.try_into()?)).unwrap()),
            (&String(ref a), &String(ref b)) => Ok(a.cmp(b)),
            (&Char(ref a), &Char(ref b)) => Ok(a.cmp(b)),
            (&Pair(ref a), &Pair(ref b)) => a.borrow().compare(&b.borrow()),
            (&Nil, &Nil) => Ok(Ordering::Equal),
            // TODO: Error type
            (a, b) => panic!("Can't compare"),
        }
    }

    // TODO: Better error handling
    // TODO: Add vector equality
    pub fn is_equal(&self, other: &Self) -> LispResult<bool> {
        use self::Value::*;
        fn within_epsilon(f1: f64, f2: f64) -> bool {
            (f1 - f2).abs() < std::f64::EPSILON
        }
        match (self, other) {
            (&Integer(ref a), &Integer(ref b)) => Ok(a == b),
            (&Symbol(ref a), &Symbol(ref b)) => Ok(a == b),
            (&Bignum(ref a), &Bignum(ref b)) => Ok(a == b),
            (&Rational(ref a), &Rational(ref b)) => Ok(a == b),
            (&Integer(ref a), &Rational(ref b)) => Ok((a * b.denom()) == *b.numer()),
            (&Rational(ref a), &Integer(ref b)) => Ok(*a.numer() == (b * a.denom())),
            (other, &Float(b)) => Ok(within_epsilon(other.try_into()?, b)),
            (&Float(b), other) => Ok(within_epsilon(b, other.try_into()?)),
            (&String(ref a), &String(ref b)) => Ok(a == b),
            (&Char(a), &Char(b)) => Ok(a == b),
            (&Pair(ref a), &Pair(ref b)) => a.borrow().is_equal(&b.borrow()),
            (&Nil, &Nil) => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn is_self_evaluating(&self) -> bool {
        use self::Value::*;
        match *self {
            Symbol(_) | Char(_) | Vector(_) | Integer(_) | Rational(_) | Float(_) | Bignum(_)
            | String(_) | Nil | Undefined => true,
            _ => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Value::Bool(a), &Value::Bool(b)) => a == b,
            (&Value::Char(a), &Value::Char(b)) => a == b,
            (&Value::Symbol(a), &Value::Symbol(b)) => a == b,
            (&Value::String(ref a), &Value::String(ref b)) => a == b,
            (&Value::Integer(a), &Value::Integer(b)) => a == b,
            (&Value::Rational(ref a), &Value::Rational(ref b)) => a == b,
            (&Value::Bignum(ref a), &Value::Bignum(ref b)) => a == b,
            (&Value::Float(a), &Value::Float(b)) => {
                // This is pretty hacky but better than not allowing floats
                // to be used as hash keys.
                // This eq is only meant to be used for hashmaps,
                // so it's not that bad.
                a.to_string() == b.to_string()
            }
            (&Value::Pair(ref a1), &Value::Pair(ref b1)) => a1 == b1,
            (&Value::Vector(ref a), &Value::Vector(ref b)) => a == b,
            (&Value::Undefined, &Value::Undefined) => true,
            (&Value::Nil, &Value::Nil) => true,
            _ => false,
        }
    }
}
impl Eq for Value {}

// NOTE: The strings are there so that (pair a b) != (cons a b) != (list a b)
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Value::Bool(v) => v.hash(state),
            Value::Integer(v) => v.hash(state),
            Value::Rational(ref v) => v.hash(state),
            Value::Bignum(ref v) => v.hash(state),
            Value::Char(v) => v.hash(state),
            Value::String(ref v) => {
                "string".hash(state);
                v.hash(state)
            }
            Value::Symbol(v) => v.hash(state),
            Value::Pair(ref ptr) => {
                "pair".hash(state);
                ptr.borrow().hash(state);
            }
            Value::ActivationFrame(ref vs) => {
                "activation".hash(state);
                for v in vs {
                    v.hash(state);
                }
            }
            Value::Continuation(ref vs, ref es, ref pcs) => {
                "continuation".hash(state);
                for v in vs {
                    v.hash(state);
                }
                // TODO: Find a way to hash EnvRefs
                // for v in es {
                //     v.hash(state);
                // }
                for v in pcs {
                    v.hash(state);
                }
            }
            Value::Vector(ref ptr) => {
                "vector".hash(state);
                for a in ptr.borrow().iter() {
                    a.hash(state);
                }
            }
            Value::Undefined => {
                "undefined".hash(state);
            }
            Value::Nil => {
                "nil".hash(state);
            }
            Value::Builtin1(sym, _) => {
                "builtin".hash(state);
                sym.hash(state);
            }
            Value::Builtin2(sym, _) => {
                "builtin".hash(state);
                sym.hash(state);
            }
            Value::Builtin3(sym, _) => {
                "builtin".hash(state);
                sym.hash(state);
            }
            Value::BuiltinN(sym, _, _) => {
                "builtin".hash(state);
                sym.hash(state);
            }
            Value::Float(v) => {
                // This is pretty bit hacky but better than not allowing floats
                // to be used as hash keys.
                // This eq is only meant to be used for hashmaps,
                // so it's not that bad.
                "float".hash(state);
                v.to_string().hash(state);
            }
            Value::Closure(offset, _, _, _) => {
                "closure".hash(state);
                offset.hash(state);
            }
        }
    }
}

pub trait LispAdd {
    fn add(&self, other: &Value) -> LispResult<Value>;
}
pub trait LispSub {
    fn sub(&self, other: &Value) -> LispResult<Value>;
}
pub trait LispNeg {
    fn neg(&self) -> LispResult<Value>;
}
pub trait LispMul {
    fn mul(&self, other: &Value) -> LispResult<Value>;
}
pub trait LispDiv {
    fn div(&self, other: &Value) -> LispResult<Value>;
}
pub trait LispIntegerDiv {
    fn integer_div(&self, other: &Value) -> LispResult<Value>;
}
pub trait LispRem {
    fn rem(&self, other: &Value) -> LispResult<Value>;
}

impl LispAdd for Value {
    fn add(&self, other: &Value) -> LispResult<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => match a.checked_add(*b) {
                Some(r) => Ok(Value::Integer(r)),
                None => Ok(Value::Bignum(BigInt::from(*a) + BigInt::from(*b))),
            },
            (Value::Integer(a), Value::Bignum(b)) => Ok(Value::Bignum(BigInt::from(*a) + b)),
            (Value::Bignum(a), Value::Integer(b)) => Ok(Value::Bignum(a + BigInt::from(*b))),
            (Value::Bignum(a), Value::Bignum(b)) => Ok(Value::Bignum(a + b)),
            (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a + b)),
            (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(b + a)),
            (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a + b)),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Ok(Value::Float(f + other))
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Ok(Value::Float(f + other))
            }
            // TODO: Error type
            (a, b) => panic!("Addition not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl LispSub for Value {
    fn sub(&self, other: &Value) -> LispResult<Value> {
        match (self, other) {
            // TODO: Check for underflow, similar to what addition does
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Integer(a), Value::Bignum(b)) => Ok(Value::Bignum(BigInt::from(*a) - b)),
            (Value::Bignum(a), Value::Integer(b)) => Ok(Value::Bignum(a - BigInt::from(*b))),
            (Value::Bignum(a), Value::Bignum(b)) => Ok(Value::Bignum(a - b)),
            (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a - b)),
            // `-b + a` because only `rational + isize` and `rational - isize` are supported
            (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(-b + a)),
            (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a - b)),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Ok(Value::Float(f - other))
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Ok(Value::Float(other - f))
            }
            // TODO: Error type
            (a, b) => panic!("Subtraction not implemented"),
        }
    }
}

impl LispNeg for Value {
    fn neg(&self) -> LispResult<Value> {
        match self {
            Value::Integer(a) => Ok(Value::Integer(-a)),
            Value::Float(a) => Ok(Value::Float(-a)),
            Value::Rational(a) => Ok(Value::Rational(-a)),
            // TODO: Error type
            a => panic!("Negation not implemented"),
        }
    }
}

impl LispMul for Value {
    fn mul(&self, other: &Value) -> LispResult<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => match a.checked_mul(*b) {
                Some(r) => Ok(Value::Integer(r)),
                None => Ok(Value::Bignum(BigInt::from(*a) * BigInt::from(*b))),
            },
            (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(b * a)),
            (Value::Integer(a), Value::Bignum(b)) => Ok(Value::Bignum(BigInt::from(*a) * b)),
            (Value::Bignum(a), Value::Integer(b)) => Ok(Value::Bignum(a * BigInt::from(*b))),
            (Value::Bignum(a), Value::Bignum(b)) => Ok(Value::Bignum(a * b)),
            (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a * b)),
            (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a * b)),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Ok(Value::Float(f * other))
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Ok(Value::Float(other * f))
            }
            // TODO: Error type
            (a, b) => panic!("Multiplication not implemented"),
        }
    }
}

impl LispDiv for Value {
    fn div(&self, other: &Value) -> LispResult<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => {
                if a % b == 0 {
                    Ok(Value::Integer(a / b))
                } else {
                    Ok(Value::Rational(Rational::new(*a, *b)))
                }
            }
            (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(Rational::from(*a) / b)),
            (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a / b)),
            (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a / b)),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().expect("TODO");
                Ok(Value::Float(f / other))
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().expect("TODO");
                Ok(Value::Float(other / f))
            }
            // TODO: Error type
            (a, b) => panic!("Division not implemented"),
        }
    }
}

impl LispRem for Value {
    fn rem(&self, other: &Value) -> LispResult<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a % b)),
            (Value::Bignum(a), Value::Integer(b)) => Ok(Value::Bignum(a % BigInt::from(*b))),
            // TODO: Error type
            (a, b) => panic!("Remainder not implemented"),
        }
    }
}

impl LispIntegerDiv for Value {
    fn integer_div(&self, other: &Value) -> LispResult<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
            (Value::Bignum(a), Value::Integer(b)) => Ok(Value::Bignum(a / b)),
            // TODO: Error type
            (a, b) => panic!("Integer Division not implemented"),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = LispError;

    fn try_from(datum: Value) -> LispResult<String> {
        match datum {
            Value::String(n) => Ok(n),
            other => Err(LispError::TypeError("convert", "string", other)),
        }
    }
}

impl TryFrom<Value> for Fsize {
    type Error = LispError;

    fn try_from(datum: Value) -> LispResult<Fsize> {
        match datum {
            Value::Integer(n) => Ok(n as Fsize),
            Value::Rational(r) => Ok((*r.numer() as Fsize) / (*r.denom() as Fsize)),
            Value::Float(r) => Ok(r),
            other => Err(LispError::TypeError("convert", "float", other)),
        }
    }
}
impl TryFrom<&Value> for Fsize {
    type Error = LispError;

    fn try_from(datum: &Value) -> LispResult<Fsize> {
        match datum {
            &Value::Integer(n) => Ok(n as Fsize),
            &Value::Rational(ref r) => Ok((*r.numer() as Fsize) / (*r.denom() as Fsize)),
            &Value::Float(r) => Ok(r),
            other => Err(LispError::TypeError("convert", "float", other.clone())),
        }
    }
}

impl TryFrom<Value> for isize {
    type Error = LispError;

    fn try_from(datum: Value) -> LispResult<isize> {
        match datum {
            Value::Integer(n) => Ok(n),
            other => Err(LispError::TypeError("convert", "integer", other)),
        }
    }
}
impl TryFrom<&Value> for isize {
    type Error = LispError;

    fn try_from(datum: &Value) -> LispResult<isize> {
        match datum {
            &Value::Integer(n) => Ok(n),
            other => Err(LispError::TypeError("convert", "integer", other.clone())),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = LispError;

    fn try_from(datum: Value) -> LispResult<usize> {
        match datum {
            Value::Integer(n) if n >= 0 => Ok(n as usize),
            other => Err(LispError::TypeError("convert", "uinteger", other)),
        }
    }
}
impl TryFrom<&Value> for usize {
    type Error = LispError;

    fn try_from(datum: &Value) -> LispResult<usize> {
        match datum {
            &Value::Integer(n) if n >= 0 => Ok(n as usize),
            other => Err(LispError::TypeError("convert", "uinteger", other.clone())),
        }
    }
}

impl TryFrom<Value> for char {
    type Error = LispError;

    fn try_from(datum: Value) -> LispResult<char> {
        match datum {
            Value::Char(c) => Ok(c),
            other => Err(LispError::TypeError("convert", "char", other)),
        }
    }
}
impl TryFrom<&Value> for char {
    type Error = LispError;

    fn try_from(datum: &Value) -> LispResult<char> {
        match datum {
            &Value::Char(c) => Ok(c),
            other => Err(LispError::TypeError("convert", "char", other.clone())),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Symbol(x) => write!(f, "{}", x),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            // TODO: Handling of space, newline, tab
            Value::Char(c) => write!(f, "#\\{}", c),
            Value::Pair(ref ptr) => {
                let pair = ptr.borrow();
                let elems = pair.collect();
                let head = &elems[..(elems.len() - 1)];
                let tail = &elems[elems.len() - 1];

                write!(f, "(")?;
                for (i, e) in head.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?
                }

                match tail {
                    &Value::Nil => (),
                    other => write!(f, " . {}", other)?,
                }

                write!(f, ")")
            }
            Value::Vector(ref elems) => {
                write!(f, "#(")?;
                for (i, e) in elems.borrow().iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?
                }
                write!(f, ")")
            }
            Value::ActivationFrame(ref elems) => {
                write!(f, "#AF(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?
                }
                write!(f, ")")
            }
            Value::Continuation(_, _, _) => write!(f, "<continuation>"),
            Value::Integer(x) => write!(f, "{}", x),
            Value::Rational(ref x) => write!(f, "{}", x),
            Value::Bignum(ref x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(ref s) => write!(f, "\"{}\"", s),
            Value::Nil => write!(f, "'()"),
            Value::Undefined => write!(f, "undefined"),
            Value::Builtin1(sym, _) => write!(f, "<builtin1 {}>", sym),
            Value::Builtin2(sym, _) => write!(f, "<builtin2 {}>", sym),
            Value::Builtin3(sym, _) => write!(f, "<builtin3 {}>", sym),
            Value::BuiltinN(sym, _, _) => write!(f, "<builtinN {}>", sym),
            Value::Closure(index, _, _, _) => write!(f, "<closure {}>", index),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Symbol(x) => write!(f, "{}", x),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            // TODO: Handling of space, newline, tab
            Value::Char(c) => write!(f, "#\\{}", c),
            Value::Pair(ref ptr) => {
                let pair = ptr.borrow();
                let elems = pair.collect();
                let head = &elems[..(elems.len() - 1)];
                let tail = &elems[elems.len() - 1];

                write!(f, "(")?;
                for (i, e) in head.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?
                }

                match tail {
                    &Value::Nil => (),
                    other => write!(f, " . {}", other)?,
                }

                write!(f, ")")
            }
            Value::Vector(ref elems) => {
                write!(f, "#(")?;
                for (i, e) in elems.borrow().iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?
                }
                write!(f, ")")
            }
            Value::ActivationFrame(ref elems) => {
                write!(f, "#AF(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?
                }
                write!(f, ")")
            }
            Value::Continuation(_, _, _) => write!(f, "<continuation>"),
            Value::Integer(x) => write!(f, "{}", x),
            Value::Rational(ref x) => write!(f, "{}", x),
            Value::Bignum(ref x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(ref s) => write!(f, "\"{}\"", s),
            Value::Nil => write!(f, "'()"),
            Value::Undefined => write!(f, "undefined"),
            Value::Builtin1(sym, _) => write!(f, "<builtin1 {}>", sym),
            Value::Builtin2(sym, _) => write!(f, "<builtin2 {}>", sym),
            Value::Builtin3(sym, _) => write!(f, "<builtin3 {}>", sym),
            Value::BuiltinN(sym, _, _) => write!(f, "<builtinN {}>", sym),
            Value::Closure(index, _, _, _) => write!(f, "<closure {}>", index),
        }
    }
}
