extern crate time;
extern crate nom;
extern crate rustyline;
extern crate rand;
extern crate bit_vec;

#[macro_use]
mod macros;

pub mod repl;
pub mod doc;
pub mod parser;
pub mod compiler;
pub mod symbol_table;

mod builtin;
mod env;
mod bignum;
mod numbers;
mod syntax_rule;
mod lexer;
mod data_structures;

use env::EnvRef;
use compiler::vm::VM;

use std::fmt;
use std::cmp::Ordering;
use std::mem;
use std::hash::{Hash, Hasher};

use std::ops::Add;
use std::ops::Sub;
use std::ops::Neg;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Rem;

use numbers::Rational;
use std::rc::Rc;
use std::cell::{Ref, RefMut, RefCell};

use data_structures::priority_queue;
use symbol_table::SymbolTable;

pub type Fsize = f64;
pub type LispResult = Result<Datum, LispErr>;

#[derive(Debug, PartialEq)]
pub enum LispErr {
    InvalidNumberOfArguments,
    InvalidList,
    InvalidTypeOfArguments,
    IndexOutOfBounds,
    DefinitionAlreadyDefined,
    DefinitionNotFound,
    IOError,
    TypeError(&'static str, &'static str, Datum)
}

impl fmt::Display for LispErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LispErr::InvalidNumberOfArguments => write!(f, "Invalid number of arguments"),
            LispErr::InvalidList => write!(f, "Invalid list"),
            LispErr::InvalidTypeOfArguments => write!(f, "Invalid types of arguments"),
            LispErr::IndexOutOfBounds => write!(f, "Index out of bounds"),
            LispErr::DefinitionNotFound => write!(f, "Definition not found"),
            LispErr::DefinitionAlreadyDefined => write!(f, "Definition is already defined"),
            LispErr::IOError => write!(f, "IO Error"),
            LispErr::TypeError(fun, expected, ref got) => {
                write!(f, "Type error evaluating {}: expected {}, got {:?}", fun, expected, got)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arity {
    Exact(u8),
    Range(u8, u8),
    Min(u8)
}

// TODO: Move the check into the LispFn impl
impl Arity {
    fn check(&self, given: usize) {
        let given = given as u8;
        match *self {
            Arity::Exact(a) => {
                if a != given {
                    panic!("expected {} arguments, got {}", a, given);
                }
            },
            Arity::Min(a) => {
                if a > given {
                    panic!("expected at least {} arguments, got {}", a, given);
                }
            },
            Arity::Range(a, b) => {
                if given < a || given > b {
                    panic!("expected between {} and {} arguments, got {}", a, b, given);
                }
            }
        }
    }
}

#[derive(Clone)]
pub enum LispFn {
    Variadic(fn(&mut [Datum], &VM) -> LispResult, Arity),
    Fixed1(fn(Datum, &VM) -> LispResult),
    Fixed2(fn(Datum, Datum, &VM) -> LispResult),
    Fixed3(fn(Datum, Datum, Datum, &VM) -> LispResult),
}

impl LispFn {
    pub fn check_arity(&self, given: usize) {
        match *self {
            LispFn::Variadic(_, ref arity) => {
                arity.check(given);
            },
            LispFn::Fixed1(_) => {
                if 1 != given {
                    panic!("expected 1 arguments, got {}", given);
                }
            },
            LispFn::Fixed2(_) => {
                if 2 != given {
                    panic!("expected 2 arguments, got {}", given);
                }
            },
            LispFn::Fixed3(_) => {
                if 3 != given {
                    panic!("expected 3 arguments, got {}", given);
                }
            },
        }
    }
}

// FIXME: This is not a good implementation
impl Hash for LispFn {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            LispFn::Variadic(_, ref arity) => {
                arity.hash(state);
            },
            LispFn::Fixed1(_) => {
                1.hash(state);
            },
            LispFn::Fixed2(_) => {
                2.hash(state);
            },
            LispFn::Fixed3(_) => {
                3.hash(state);
            },
        }
    }
}

impl fmt::Debug for LispFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LispFn")
    }
}

// FIXME: Implement real equality check
// based on name & arity
impl PartialEq for LispFn {
    fn eq(&self, _other: &LispFn) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pair(pub Datum, pub Datum);

impl Pair {
    pub fn compare(&self, other: &Pair) -> Result<Ordering, LispErr> {
        let res1 = self.0.compare(&other.0)?;
        if res1 == Ordering::Equal {
            self.1.compare(&other.1)
        } else {
            Ok(res1)
        }
    }

    pub fn is_equal(&self, other: &Pair) -> Result<bool, LispErr> {
        let res1 = self.0.is_equal(&other.0)?;
        if res1 == false {
            return Ok(false);
        }

        self.1.is_equal(&other.1)
    }

    // TODO: Find a cleaner way to do this
    pub fn collect(&self) -> Vec<Datum> {
        let mut cur = Rc::new(RefCell::new(self.clone()));
        let mut res = Vec::new();

        loop {
            let a = cur.borrow().0.clone();
            let b = cur.borrow().1.clone();

            res.push(a);
            match b {
                Datum::Pair(ref ptr) => {
                    cur = ptr.clone();
                },
                other => {
                    res.push(other);
                    break;
                }
            }
        }

        res
    }

    pub fn collect_list(&self) -> Result<Vec<Datum>, LispErr> {
        let mut v = self.collect();
        let last = v.pop().unwrap();

        if Datum::Nil == last {
            Ok(v)
        } else {
            Err(LispErr::InvalidList)
        }
    }
}

pub type PairRef = Rc<RefCell<Pair>>;

pub type Vector = Vec<Datum>;
pub type VectorRef = Rc<RefCell<Vector>>;
pub type PriorityQueueRef = Rc<RefCell<priority_queue::PriorityQueue>>;

#[derive(Clone, Debug)]
pub enum Datum {
    Bool(bool),
    Integer(isize),
    Rational(numbers::Rational),
    Float(Fsize),
    Bignum(bignum::Bignum),
    Char(char),
    String(String),
    Symbol(Symbol),
    Pair(PairRef),
    Vector(VectorRef),
    Builtin(LispFn),
    PriorityQueue(PriorityQueueRef),
    // TODO: Switch this to rc refcells
    Undefined,
    Nil,
    // offset, arity, dotted?, env
    Closure(usize, usize, bool, EnvRef)
}

#[derive(Clone, Debug)]
pub enum Expression {
    Bool(bool),
    Integer(isize),
    Float(Fsize),
    Rational(Rational),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expression>),
    DottedList(Vec<Expression>, Box<Expression>),
    Vector(Vec<Expression>),
    Undefined,
    Nil,
}

impl Expression {
    fn to_datum(self, st: &mut SymbolTable) -> Datum {
        match self {
            Expression::Bool(v) => Datum::Bool(v),
            Expression::Integer(v) => Datum::Integer(v),
            Expression::Float(v) => Datum::Float(v),
            Expression::Rational(r) => Datum::Rational(r),
            Expression::Char(v) => Datum::Char(v),
            Expression::String(v) => Datum::String(v),
            Expression::Symbol(v) => Datum::Symbol(st.insert(&v)),
            Expression::List(es) => {
                let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
                Datum::make_list_from_vec(ds)
            },
            Expression::DottedList(es, tail) => {
                let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
                Datum::make_dotted_list_from_vec(ds, tail.to_datum(st))
            },
            Expression::Vector(es) => {
                let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
                Datum::make_vector_from_vec(ds)
            },
            Expression::Undefined => Datum::Undefined,
            Expression::Nil => Datum::Nil,
        }
    }

    fn as_list(&self) -> Result<Vec<Expression>, LispErr> {
        match self {
            &Expression::List(ref elems) => Ok(elems.clone()),
            &Expression::Nil => Ok(Vec::new()),
            a => panic!("Can't convert {} to a list", a)
        }
    }

    fn as_symbol(&self) -> Result<String, LispErr> {
        match self {
            &Expression::Symbol(ref s) => Ok(s.clone()),
            // TODO: Adapt lisp errors
            other => panic!("Experted {} to be a symbol", other),
        }
    }

    fn is_numeric(&self) -> bool {
        match *self {
            Expression::Integer(_) => true,
            Expression::Float(_) => true,
            _ => false
        }
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Expression) -> bool {
        match (self, other) {
            (&Expression::Bool(a), &Expression::Bool(b)) => a == b,
            (&Expression::Char(a), &Expression::Char(b)) => a == b,
            (&Expression::Symbol(ref a), &Expression::Symbol(ref b)) => a == b,
            (&Expression::String(ref a), &Expression::String(ref b)) => a == b,
            (&Expression::Integer(a), &Expression::Integer(b)) => a == b,
            (&Expression::Float(a), &Expression::Float(b)) => {
                // This is pretty bit hacky but better than not allowing floats
                // to be used as hash keys.
                // This eq is only meant to be used for hashmaps,
                // so it's not that bad.
                a.to_string() == b.to_string()
            },
            (&Expression::List(ref a1), &Expression::List(ref b1)) => {
                a1 == b1
            },
            (&Expression::DottedList(ref a1, ref a2),
             &Expression::DottedList(ref b1, ref b2)) => {
                a1 == b1 && a2 == b2
            },
            (&Expression::Vector(ref a), &Expression::Vector(ref b)) => a == b,
            (&Expression::Undefined, &Expression::Undefined) => true,
            (&Expression::Nil, &Expression::Nil) => true,
            _ => false
        }
    }
}
impl Eq for Expression {}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Symbol(ref v) => write!(f, "{}", v),
            Expression::Bool(v) => {
                if v {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            },
            Expression::Char(c) => write!(f, "#\\{}", c),
            Expression::List(ref elems) => {
                let inner : Vec<String> = elems.iter().map(
                    |e| e.to_string()
                ).collect();
                write!(f, "({})", inner.join(" "))
            },
            Expression::DottedList(ref elems, ref tail) => {
                let inner : Vec<String> = elems.iter().map(
                    |e| e.to_string()
                ).collect();
                write!(f, "({} . {})", inner.join(" "), tail)
            },
            Expression::Vector(ref elems) => {
                let inner : Vec<String> = elems.iter().map(
                    |e| e.to_string()
                ).collect();
                write!(f, "#({})", inner.join(" "))
            },
            Expression::Integer(x) => write!(f, "{}", x),
            Expression::Rational(ref x) => write!(f, "{}", x),
            Expression::Float(x) => write!(f, "{}", x),
            Expression::String(ref s) => write!(f, "\"{}\"", s),
            Expression::Nil => write!(f, "'()"),
            Expression::Undefined => write!(f, "undefined"),
        }
    }
}

impl PartialEq for Datum {
    fn eq(&self, other: &Datum) -> bool {
        match (self, other) {
            (&Datum::Bool(a), &Datum::Bool(b)) => a == b,
            (&Datum::Char(a), &Datum::Char(b)) => a == b,
            (&Datum::Symbol(a), &Datum::Symbol(b)) => a == b,
            (&Datum::String(ref a), &Datum::String(ref b)) => a == b,
            (&Datum::Integer(a), &Datum::Integer(b)) => a == b,
            (&Datum::Rational(ref a), &Datum::Rational(ref b)) => a == b,
            (&Datum::Bignum(ref a), &Datum::Bignum(ref b)) => a == b,
            (&Datum::Float(a), &Datum::Float(b)) => {
                // This is pretty bit hacky but better than not allowing floats
                // to be used as hash keys.
                // This eq is only meant to be used for hashmaps,
                // so it's not that bad.
                a.to_string() == b.to_string()
            },
            (&Datum::Pair(ref a1), &Datum::Pair(ref b1)) => {
                a1 == b1
            },
            (&Datum::Vector(ref a), &Datum::Vector(ref b)) => a == b,
            (&Datum::Undefined, &Datum::Undefined) => true,
            (&Datum::Nil, &Datum::Nil) => true,
            _ => false
        }
    }
}
impl Eq for Datum {}

// NOTE: The strings are there so that (pair a b) != (cons a b) != (list a b)
impl Hash for Datum {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Datum::Bool(v) => v.hash(state),
            Datum::Integer(v) => v.hash(state),
            Datum::Rational(ref v) => v.hash(state),
            Datum::Bignum(ref v) => v.hash(state),
            Datum::Char(v) => v.hash(state),
            Datum::String(ref v) => {
                "string".hash(state);
                v.hash(state)
            },
            Datum::Symbol(v) => v.hash(state),
            Datum::Pair(ref ptr) => {
                "pair".hash(state);
                ptr.borrow().hash(state);
            },
            Datum::Vector(ref ptr) => {
                "vector".hash(state);
                for a in ptr.borrow().iter() {
                    a.hash(state);
                }
            },
            Datum::Undefined => {
                "undefined".hash(state);
            },
            Datum::Nil => {
                "nil".hash(state);
            },
            Datum::Builtin(ref f) => {
                f.hash(state);
            },
            Datum::PriorityQueue(ref ptr) => {
                // Just test pointer equality
                "priority_queue".hash(state);
                let ptr = Rc::into_raw(ptr.clone());
                ptr.hash(state);
            },
            Datum::Float(v) => {
                // This is pretty bit hacky but better than not allowing floats
                // to be used as hash keys.
                // This eq is only meant to be used for hashmaps,
                // so it's not that bad.
                "float".hash(state);
                v.to_string().hash(state);
            },
            Datum::Closure(offset, _, _, _) => {
                "closure".hash(state);
                offset.hash(state);
            }
        }
    }
}

impl Add for Datum {
    type Output = Datum;

    // TODO: Allow these to return errors
    fn add(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => {
                match a.checked_add(b) {
                    Some(r) => Datum::Integer(r),
                    None => {
                       Datum::Bignum(
                           bignum::Bignum::new(a) +
                           bignum::Bignum::new(b)
                        )
                    }
                }
            },
            (Datum::Integer(a), Datum::Bignum(b)) => {
                Datum::Bignum(bignum::Bignum::new(a) + b)
            },
            (Datum::Bignum(a), Datum::Integer(b)) => {
                Datum::Bignum(a + bignum::Bignum::new(b))
            },
            (Datum::Bignum(a), Datum::Bignum(b)) => {
                Datum::Bignum(a + b)
            },
            (Datum::Rational(a), Datum::Integer(b)) => (a + b).reduce(),
            (Datum::Integer(a), Datum::Rational(b)) => (a + b).reduce(),
            (Datum::Rational(a), Datum::Rational(b)) => (a + b).reduce(),
            (Datum::Float(f), other) => Datum::Float(f + other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(f + other.as_float().unwrap()),
            (a, b) => panic!("Addition not implemented for {:?} and {:?}", a, b)
        }
    }
}

impl Sub for Datum {
    type Output = Datum;

    fn sub(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => Datum::Integer(a - b),
            (Datum::Integer(a), Datum::Bignum(b)) => {
                Datum::Bignum(bignum::Bignum::new(a) - b)
            },
            (Datum::Bignum(a), Datum::Integer(b)) => {
                Datum::Bignum(a - bignum::Bignum::new(b))
            },
            (Datum::Bignum(a), Datum::Bignum(b)) => {
                Datum::Bignum(a - b)
            },
            (Datum::Rational(a), Datum::Integer(b)) => (a - b).reduce(),
            (Datum::Integer(a), Datum::Rational(b)) => (a - b).reduce(),
            (Datum::Rational(a), Datum::Rational(b)) => (a - b).reduce(),
            (Datum::Float(f), other) => Datum::Float(f - other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(other.as_float().unwrap() - f),
            (a, b) => panic!("Subtraction not implemented for {:?} and {:?}", a, b)
        }
    }
}

impl Neg for Datum {
    type Output = Datum;

    fn neg(self) -> Datum {
        match self {
            Datum::Integer(a) => Datum::Integer(-a),
            Datum::Float(a) => Datum::Float(-a),
            Datum::Rational(a) => Datum::Rational(-a),
            a => panic!("Negation not implemented for {:?}", a)
        }
    }
}

impl Mul for Datum {
    type Output = Datum;

    fn mul(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => {
                match a.checked_mul(b) {
                    Some(r) => Datum::Integer(r),
                    None => {
                       Datum::Bignum(
                           bignum::Bignum::new(a) *
                           bignum::Bignum::new(b)
                        )
                    }
                }
            },
            (Datum::Integer(a), Datum::Rational(b)) => (a * b).reduce(),
            (Datum::Integer(a), Datum::Bignum(b)) => {
                Datum::Bignum(bignum::Bignum::new(a) * b)
            },
            (Datum::Bignum(a), Datum::Integer(b)) => {
                Datum::Bignum(a * bignum::Bignum::new(b))
            },
            (Datum::Bignum(a), Datum::Bignum(b)) => {
                Datum::Bignum(a * b)
            },
            (Datum::Rational(a), Datum::Integer(b)) => (a * b).reduce(),
            (Datum::Rational(a), Datum::Rational(b)) => (a * b).reduce(),
            (Datum::Float(f), other) => Datum::Float(f * other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(f * other.as_float().unwrap()),
            (a, b) => panic!("Multiplication not implemented for {:?} and {:?}", a, b)
        }
    }
}

impl Div for Datum {
    type Output = Datum;

    fn div(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => {
                if a % b == 0 {
                    Datum::Integer(a / b)
                } else {
                    Datum::Rational(Rational::new(a, b))
                }
            },
            (Datum::Integer(a), Datum::Rational(b)) => (a / b).reduce(),
            (Datum::Rational(a), Datum::Integer(b)) => (a / b).reduce(),
            (Datum::Rational(a), Datum::Rational(b)) => (a / b).reduce(),
            // TODO: Maybe create a second trait for "lossy" division?
            // (Datum::Bignum(a), Datum::Integer(b)) => Datum::Bignum(a / b),
            (Datum::Float(f), other) => Datum::Float(f / other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(other.as_float().unwrap() / f),
            (a, b) => panic!("Division not implemented for {:?} and {:?}", a, b)
        }
    }
}

impl Rem for Datum {
    type Output = Datum;

    fn rem(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => Datum::Integer(a % b),
            (Datum::Bignum(a), Datum::Integer(b)) => Datum::Integer(a % b),
            (a, b) => panic!("Remainder not implemented for {:?} and {:?}", a, b)
        }
    }
}

impl Rem<isize> for Datum {
    type Output = isize;

    fn rem(self, other: isize) -> isize {
        match (self, other) {
            (Datum::Integer(a), b) => a % b,
            (Datum::Bignum(a), b) => a % b,
            (a, b) => panic!("Remainder not implemented for {:?} and {:?}", a, b)
        }
    }
}

pub trait IntegerDiv<RHS=Self> {
    type Output: Sized;

    fn int_div(self, other: RHS) -> Self::Output;
}

impl IntegerDiv for Datum {
    type Output = Datum;

    fn int_div(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => Datum::Integer(a / b),
            (Datum::Bignum(a), Datum::Integer(b)) => Datum::Bignum(a.int_div(b)),
            (a, b) => panic!("Integer Division not implemented for {:?} and {:?}", a, b)
        }
    }
}

impl Datum {
    fn make_list(elems: &mut [Datum]) -> Datum {
        let mut res = Datum::Nil;
        for next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Datum::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    fn make_list_from_vec(elems: Vec<Datum>) -> Datum {
        let mut res = Datum::Nil;
        for mut next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Datum::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    fn make_vector(elems: &mut [Datum]) -> Datum {
        Datum::Vector(Rc::new(RefCell::new(elems.to_vec())))
    }

    fn make_vector_from_vec(elems: Vec<Datum>) -> Datum {
        Datum::Vector(Rc::new(RefCell::new(elems)))
    }

    fn make_dotted_list_from_vec(elems: Vec<Datum>, tail: Datum) -> Datum {
        let mut res = tail;
        for mut next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Datum::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    fn make_pair(fst: Datum, rst: Datum) -> Datum {
        let pair = Pair(fst, rst);
        Datum::Pair(Rc::new(RefCell::new(pair)))
    }

    fn make_priority_queue(pq: priority_queue::PriorityQueue) -> Datum {
        Datum::PriorityQueue(Rc::new(RefCell::new(pq)))
    }

    fn is_pair(&self) -> bool {
        match *self {
          Datum::Pair(_) => true,
          _ => false,
        }
    }

    fn is_nil(&self) -> bool {
        match *self {
          Datum::Nil => true,
          _ => false,
        }
    }

    fn take(&mut self) -> Datum {
        mem::replace(self, Datum::Undefined)
    }

    fn as_float(&self) -> Result<Fsize, LispErr> {
        match self {
            &Datum::Integer(n) => Ok(n as Fsize),
            &Datum::Rational(ref r) => Ok((r.num as Fsize) / (r.denom as Fsize)),
            &Datum::Float(r) => Ok(r),
            other => Err(LispErr::TypeError("convert", "float", other.clone()))
        }
    }

    fn as_integer(&self) -> Result<isize, LispErr> {
        match self {
            &Datum::Integer(n) => Ok(n),
            other => Err(LispErr::TypeError("convert", "integer", other.clone()))
        }
    }

    fn as_uinteger(&self) -> Result<usize, LispErr> {
        match self {
            &Datum::Integer(n) => {
                if n >= 0 {
                    Ok(n as usize)
                } else {
                    Err(LispErr::TypeError("convert", "uinteger", self.clone()))
                }
            },
            other => Err(LispErr::TypeError("convert", "uinteger", other.clone()))
        }
    }

    fn as_symbol(&self) -> Result<Symbol, LispErr> {
        match self {
            &Datum::Symbol(n) => Ok(n),
            other => Err(LispErr::TypeError("convert", "symbol", other.clone()))
        }
    }

    fn as_string(&self) -> Result<String, LispErr> {
        match self {
            &Datum::String(ref n) => Ok(n.clone()),
            other => Err(LispErr::TypeError("convert", "string", other.clone()))
        }
    }

    fn as_char(&self) -> Result<char, LispErr> {
        match self {
            &Datum::Char(n) => Ok(n),
            other => Err(LispErr::TypeError("convert", "char", other.clone()))
        }
    }

    fn as_pair(&self) -> Result<Ref<Pair>, LispErr> {
        match self {
            &Datum::Pair(ref ptr) => Ok(ptr.borrow()),
            other => Err(LispErr::TypeError("convert", "pair", other.clone()))
        }
    }

    fn as_mut_pair(&self) -> Result<RefMut<Pair>, LispErr> {
        match self {
            &Datum::Pair(ref ptr) => Ok(ptr.borrow_mut()),
            other => Err(LispErr::TypeError("convert", "pair", other.clone()))
        }
    }

    fn as_priority_queue(&self) -> Result<Ref<priority_queue::PriorityQueue>, LispErr> {
        match self {
            &Datum::PriorityQueue(ref ptr) => Ok(ptr.borrow()),
            other => Err(LispErr::TypeError("convert", "priority-queue", other.clone()))
        }
    }

    fn as_mut_priority_queue(&self) -> Result<RefMut<priority_queue::PriorityQueue>, LispErr> {
        match self {
            &Datum::PriorityQueue(ref ptr) => Ok(ptr.borrow_mut()),
            other => Err(LispErr::TypeError("convert", "priority-queue", other.clone()))
        }
    }

    fn as_vector(&self) -> Result<Ref<Vector>, LispErr> {
        match self {
            &Datum::Vector(ref ptr) => Ok(ptr.borrow()),
            other => Err(LispErr::TypeError("convert", "vector", other.clone()))
        }
    }

    fn as_mut_vector(&self) -> Result<RefMut<Vector>, LispErr> {
        match self {
            &Datum::Vector(ref ptr) => Ok(ptr.borrow_mut()),
            other => Err(LispErr::TypeError("convert", "vector", other.clone()))
        }
    }

    // TODO: Remove some borrow & clones
    fn as_list(&self) -> Result<Vec<Datum>, LispErr> {
        match self {
            &Datum::Pair(ref ptr) => {
                let mut cur = ptr.clone();
                let mut res = Vec::new();
                loop {
                    res.push(cur.borrow().0.clone());
                    let rst = cur.borrow().1.clone();
                    match rst {
                        Datum::Pair(ptr) => {
                            cur = ptr.clone();
                            continue
                        },
                        Datum::Nil => {
                            break
                        },
                        _ => {
                            return Err(LispErr::InvalidList)
                        }
                    }
                }
                Ok(res)
            },
            &Datum::Nil => Ok(Vec::new()),
            a => panic!("Can't convert {:?} to a list", a)
        }
    }

    fn is_false(&self) -> bool {
        match self {
            &Datum::Nil => true,
            &Datum::Bool(false) => true,
            _ => false
        }
    }

    fn is_true(&self) -> bool {
        match self {
            &Datum::Nil => false,
            &Datum::Bool(false) => false,
            _ => true
        }
    }

    // TODO: Better error handling
    // TODO: Distinction between `=`, `eq?`, `eqv?` and `equal?`
    fn compare(&self, other: &Datum) -> Result<Ordering, LispErr> {
        match (self, other) {
            (&Datum::Integer(ref a), &Datum::Integer(ref b)) => Ok(a.cmp(b)),
            (&Datum::Symbol(ref a), &Datum::Symbol(ref b)) => Ok(a.cmp(b)),
            (&Datum::Bignum(ref a), &Datum::Bignum(ref b)) => Ok(a.cmp(b)),
            (&Datum::Integer(a), &Datum::Bignum(ref b)) => Ok(bignum::Bignum::new(a).cmp(b)),
            (&Datum::Bignum(ref a), &Datum::Integer(b)) => Ok(a.cmp(&bignum::Bignum::new(b))),
            (&Datum::Rational(ref a), &Datum::Rational(ref b)) => Ok(
                (a.num * b.denom).cmp(&(b.num * a.denom))
            ),
            (&Datum::Integer(ref a), &Datum::Rational(ref b)) => Ok(
                (a *  b.denom).cmp(&(b.num))
            ),
            (&Datum::Rational(ref a), &Datum::Integer(ref b)) => Ok(
                a.num.cmp(&(b *  a.denom))
            ),
            (ref other, &Datum::Float(ref b)) => {
                Ok((other.as_float()?).partial_cmp(b).unwrap())
            },
            (&Datum::Float(ref b), ref other) => {
                Ok(b.partial_cmp(&other.as_float()?).unwrap())
            },
            (&Datum::String(ref a), &Datum::String(ref b)) => Ok(a.cmp(b)),
            (&Datum::Char(ref a), &Datum::Char(ref b)) => Ok(a.cmp(b)),
            (&Datum::Pair(ref a), &Datum::Pair(ref b)) => a.borrow().compare(&b.borrow()),
            (&Datum::Nil, &Datum::Nil) => Ok(Ordering::Equal),
            (a, b) => panic!("Can't compare {:?} and {:?}", a, b)
        }
    }

    // TODO: Better error handling
    // TODO: Add vector equality
    fn is_equal(&self, other: &Datum) -> Result<bool, LispErr> {
        match (self, other) {
            (&Datum::Integer(ref a), &Datum::Integer(ref b)) => Ok(a == b),
            (&Datum::Symbol(ref a), &Datum::Symbol(ref b)) => Ok(a == b),
            (&Datum::Bignum(ref a), &Datum::Bignum(ref b)) => Ok(a == b),
            (&Datum::Rational(ref a), &Datum::Rational(ref b)) => Ok(
                (a.num * b.denom) == (b.num * a.denom)
            ),
            (&Datum::Integer(ref a), &Datum::Rational(ref b)) => Ok(
                (a * b.denom) == b.num
            ),
            (&Datum::Rational(ref a), &Datum::Integer(ref b)) => Ok(
                a.num == (b *  a.denom)
            ),
            (ref other, &Datum::Float(b)) => {
                Ok((other.as_float()?) == b)
            },
            (&Datum::Float(b), ref other) => {
                Ok(b == (other.as_float()?))
            },
            (&Datum::String(ref a), &Datum::String(ref b)) => Ok(a == b),
            (&Datum::Char(a), &Datum::Char(b)) => Ok(a == b),
            (&Datum::Pair(ref a), &Datum::Pair(ref b)) => a.borrow().is_equal(&b.borrow()),
            (&Datum::Nil, &Datum::Nil) => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn is_self_evaluating(&self) -> bool {
        match *self {
            Datum::Symbol(_) => true,
            Datum::Char(_) => true,
            Datum::Vector(_) => true,
            Datum::Integer(_) => true,
            Datum::Rational(_) => true,
            Datum::Float(_) => true,
            Datum::Bignum(_) => true,
            Datum::String(_) => true,
            Datum::Nil => true,
            Datum::Undefined => true,
            _ => false,
        }
    } 

    fn to_string(&self, symbol_table: &symbol_table::SymbolTable) -> String {
        match *self {
            Datum::Symbol(x) => symbol_table.lookup(x),
            Datum::Bool(x) => {
                if x {
                    String::from("#t")
                } else {
                    String::from("#f")
                }
            },
            Datum::Char(c) => format!("#\\{}", c),
            Datum::Pair(ref ptr) => {
                let pair = ptr.borrow();
                let elems = pair.collect();
                let head = &elems[..(elems.len() - 1)];
                let tail = &elems[elems.len() - 1];

                let mut result = String::new();
                result.push_str("(");

                for (i, e) in head.iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table));
                }

                match tail {
                    &Datum::Nil => {
                        result.push_str(")");
                    },
                    other => {
                        result.push_str(" . ");
                        result.push_str(&other.to_string(symbol_table));
                        result.push_str(")");
                    },
                }

                format!("{}", result)
            },
            Datum::PriorityQueue(ref _ptr) => {
                // TODO: Display the actual elements
                format!("#<priority queue>")
            }
            Datum::Vector(ref elems) => {
                let mut result = String::new();
                result.push_str("#(");
                for (i, e) in elems.borrow().iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table));
                }
                result.push_str(")");
                format!("{}", result)
            },
            Datum::Integer(x) => format!("{}", x),
            Datum::Rational(ref x) => format!("{}", x),
            Datum::Bignum(ref x) => format!("{}", x),
            Datum::Float(x) => format!("{}", x),
            Datum::String(ref s) => format!("\"{}\"", s),
            Datum::Nil => format!("'()"),
            Datum::Undefined => format!("undefined"),
            Datum::Builtin(_) => format!("<builtin>"),
            Datum::Closure(index, _, _, _) => format!("<closure {}>", index),
        }
    }
}

pub type Symbol = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);