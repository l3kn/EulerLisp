#[macro_use]
extern crate lisp_macros;

#[macro_use]
mod macros;

pub mod code_formatter;
pub mod compiler;
pub mod debugger;
pub mod doc;
pub mod evaluator;
pub mod parser;
pub mod repl;
pub mod symbol_table;

mod builtin;
mod env;
mod instruction;
mod lexer;
mod syntax_rule;
mod vm;

use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::rc::Rc;
use std::{fmt, mem};

use num::{BigInt, Rational};

use crate::env::EnvRef;
use crate::symbol_table::SymbolTable;
use crate::vm::VM;

pub type Fsize = f64;
pub type LispResult<T> = Result<T, LispErr>;

// TODO: Combine with `LispErr`
#[derive(Debug)]
pub enum LispError {
    LexerError(lexer::LexerError),
    ParserError(parser::ParserError),
    FormatterError(code_formatter::FormatterError),
}

#[derive(Debug, PartialEq)]
pub enum CompilerError {
    UndefinedVariable(String),
    ReservedName(String),
    NonSelfEvaluatingConstant(String),
    ConstantReassignment(String),
    NoMatchingMacroPattern(Expression),
    InvalidFunctionArgument(Expression),
    IncorrectPrimitiveArity(String, usize, usize),
    InvalidInternalDefinition,
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompilerError::UndefinedVariable(ref v) => write!(f, "Undefined variable {}", v),
            CompilerError::ReservedName(ref v) => write!(f, "{} is a reserved name", v),
            CompilerError::NonSelfEvaluatingConstant(ref v) => {
                write!(f, "constant {} is not self-evaluating", v)
            }
            CompilerError::NoMatchingMacroPattern(ref v) => {
                write!(f, "no matching macro pattern for {}", v)
            }
            CompilerError::InvalidFunctionArgument(ref v) => {
                write!(f, "{} is not a valid function argument", v)
            }
            CompilerError::ConstantReassignment(ref v) => {
                write!(f, "can not reassign the constant {}", v)
            }
            CompilerError::IncorrectPrimitiveArity(ref v, e, g) => write!(
                f,
                "incorrect arity for primitive {}, expected {}, got {}",
                v, e, g
            ),
            CompilerError::InvalidInternalDefinition => write!(
                f,
                "internal definition must appear at the beginning of the body"
            ),
        }
    }
}

impl From<CompilerError> for LispErr {
    fn from(error: CompilerError) -> Self {
        LispErr::CompilerError(error)
    }
}

#[derive(Debug, PartialEq)]
pub enum LispErr {
    InvalidNumberOfArguments,
    InvalidList,
    InvalidTypeOfArguments,
    IndexOutOfBounds,
    DefinitionAlreadyDefined,
    DefinitionNotFound,
    IOError,
    CompilerError(CompilerError),
    TypeError(&'static str, &'static str, Value),
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
            LispErr::CompilerError(ref e) => write!(f, "Compiler Error, {}", e),
            LispErr::TypeError(fun, expected, ref got) => write!(
                f,
                "Type error evaluating {}: expected {}, got {:?}",
                fun, expected, got
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arity {
    Exact(u8),
    Range(u8, u8),
    Min(u8),
}

impl Arity {
    fn check(&self, given: usize) {
        let given = given as u8;
        match *self {
            Arity::Exact(a) => {
                if a != given {
                    panic!("expected {} arguments, got {}", a, given);
                }
            }
            Arity::Min(a) => {
                if a > given {
                    panic!("expected at least {} arguments, got {}", a, given);
                }
            }
            Arity::Range(a, b) => {
                if given < a || given > b {
                    panic!("expected between {} and {} arguments, got {}", a, b, given);
                }
            }
        }
    }
}

pub type LispFn1 = fn(Value, &VM) -> LispResult<Value>;
pub type LispFn2 = fn(Value, Value, &VM) -> LispResult<Value>;
pub type LispFn3 = fn(Value, Value, Value, &VM) -> LispResult<Value>;
pub type LispFnN = fn(&mut [Value], &VM) -> LispResult<Value>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LispFnType {
    Fixed1,
    Fixed2,
    Fixed3,
    Variadic,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pair(pub Value, pub Value);

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
        if !self.0.is_equal(&other.0)? {
            return Ok(false);
        }

        self.1.is_equal(&other.1)
    }

    // TODO: Find a cleaner way to do this
    pub fn collect(&self) -> Vec<Value> {
        let mut cur = Rc::new(RefCell::new(self.clone()));
        let mut res = Vec::new();

        loop {
            let a = cur.borrow().0.clone();
            let b = cur.borrow().1.clone();
            res.push(a);

            match b {
                Value::Pair(ref ptr) => cur = ptr.clone(),
                other => {
                    res.push(other);
                    break;
                }
            }
        }

        res
    }

    pub fn collect_list(&self) -> Result<Vec<Value>, LispErr> {
        let mut v = self.collect();
        let last = v.pop().unwrap();

        if Value::Nil == last {
            Ok(v)
        } else {
            Err(LispErr::InvalidList)
        }
    }
}

pub type PairRef = Rc<RefCell<Pair>>;

pub type Vector = Vec<Value>;
pub type VectorRef = Rc<RefCell<Vector>>;

#[derive(Clone, Debug)]
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
    Builtin(LispFnType, u16, Arity),
    ActivationFrame(Vec<Value>),
    Undefined,
    Nil,
    // offset, arity, dotted?, env
    Closure(usize, usize, bool, EnvRef),
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
    fn to_datum(self, st: &mut SymbolTable) -> Value {
        match self {
            Expression::Bool(v) => Value::Bool(v),
            Expression::Integer(v) => Value::Integer(v),
            Expression::Float(v) => Value::Float(v),
            Expression::Rational(r) => Value::Rational(r),
            Expression::Char(v) => Value::Char(v),
            Expression::String(v) => Value::String(v),
            Expression::Symbol(v) => Value::Symbol(st.insert(&v)),
            Expression::List(es) => {
                let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
                Value::make_list_from_vec(ds)
            }
            Expression::DottedList(es, tail) => {
                let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
                Value::make_dotted_list_from_vec(ds, tail.to_datum(st))
            }
            Expression::Vector(es) => {
                let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
                Value::make_vector_from_vec(ds)
            }
            Expression::Undefined => Value::Undefined,
            Expression::Nil => Value::Nil,
        }
    }

    fn as_list(&self) -> Result<Vec<Expression>, LispErr> {
        match self {
            &Expression::List(ref elems) => Ok(elems.clone()),
            &Expression::Nil => Ok(Vec::new()),
            a => panic!("Can't convert {} to a list", a),
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
            _ => false,
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
            }
            (&Expression::List(ref a1), &Expression::List(ref b1)) => a1 == b1,
            (&Expression::DottedList(ref a1, ref a2), &Expression::DottedList(ref b1, ref b2)) => {
                a1 == b1 && a2 == b2
            }
            (&Expression::Vector(ref a), &Expression::Vector(ref b)) => a == b,
            (&Expression::Undefined, &Expression::Undefined) => true,
            (&Expression::Nil, &Expression::Nil) => true,
            _ => false,
        }
    }
}
impl Eq for Expression {}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Symbol(ref v) => write!(f, "{}", v),
            Expression::Bool(true) => write!(f, "#t"),
            Expression::Bool(false) => write!(f, "#f"),
            Expression::Char(c) => write!(f, "#\\{}", c),
            Expression::List(ref elems) => {
                let inner: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", inner.join(" "))
            }
            Expression::DottedList(ref elems, ref tail) => {
                let inner: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
                write!(f, "({} . {})", inner.join(" "), tail)
            }
            Expression::Vector(ref elems) => {
                let inner: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
                write!(f, "#({})", inner.join(" "))
            }
            Expression::Integer(x) => write!(f, "{}", x),
            Expression::Rational(ref x) => write!(f, "{}", x),
            Expression::Float(x) => write!(f, "{}", x),
            Expression::String(ref s) => write!(f, "\"{}\"", s),
            Expression::Nil => write!(f, "'()"),
            Expression::Undefined => write!(f, "undefined"),
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
            Value::Builtin(ref typ, index, ref arity) => {
                "builtin".hash(state);
                typ.hash(state);
                index.hash(state);
                arity.hash(state);
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

impl Add for Value {
    type Output = Value;

    // TODO: Allow these to return errors
    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => match a.checked_add(b) {
                Some(r) => Value::Integer(r),
                None => Value::Bignum(BigInt::from(a) + BigInt::from(b)),
            },
            (Value::Integer(a), Value::Bignum(b)) => Value::Bignum(BigInt::from(a) + b),
            (Value::Bignum(a), Value::Integer(b)) => Value::Bignum(a + BigInt::from(b)),
            (Value::Bignum(a), Value::Bignum(b)) => Value::Bignum(a + b),
            (Value::Rational(a), Value::Integer(b)) => Value::Rational(a + b),
            (Value::Integer(a), Value::Rational(b)) => Value::Rational(b + a),
            (Value::Rational(a), Value::Rational(b)) => Value::Rational(a + b),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(f + other)
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(f + other)
            }
            (a, b) => panic!("Addition not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Integer(a), Value::Bignum(b)) => Value::Bignum(BigInt::from(a) - b),
            (Value::Bignum(a), Value::Integer(b)) => Value::Bignum(a - BigInt::from(b)),
            (Value::Bignum(a), Value::Bignum(b)) => Value::Bignum(a - b),
            (Value::Rational(a), Value::Integer(b)) => Value::Rational(a - b),
            // `-b + a` because only `rational + isize` and `rational - isize` are supported
            (Value::Integer(a), Value::Rational(b)) => Value::Rational(-b + a),
            (Value::Rational(a), Value::Rational(b)) => Value::Rational(a - b),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(f - other)
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(other - f)
            }
            (a, b) => panic!("Subtraction not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Integer(a) => Value::Integer(-a),
            Value::Float(a) => Value::Float(-a),
            Value::Rational(a) => Value::Rational(-a),
            a => panic!("Negation not implemented for {:?}", a),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => match a.checked_mul(b) {
                Some(r) => Value::Integer(r),
                None => Value::Bignum(BigInt::from(a) * BigInt::from(b)),
            },
            (Value::Integer(a), Value::Rational(b)) => Value::Rational(b * a),
            (Value::Integer(a), Value::Bignum(b)) => Value::Bignum(BigInt::from(a) * b),
            (Value::Bignum(a), Value::Integer(b)) => Value::Bignum(a * BigInt::from(b)),
            (Value::Bignum(a), Value::Bignum(b)) => Value::Bignum(a * b),
            (Value::Rational(a), Value::Integer(b)) => Value::Rational(a * b),
            (Value::Rational(a), Value::Rational(b)) => Value::Rational(a * b),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(f * other)
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(other * f)
            }
            (a, b) => panic!("Multiplication not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => {
                if a % b == 0 {
                    Value::Integer(a / b)
                } else {
                    Value::Rational(Rational::new(a, b))
                }
            }
            (Value::Integer(a), Value::Rational(b)) => Value::Rational(Rational::from(a) / b),
            (Value::Rational(a), Value::Integer(b)) => Value::Rational(a / b),
            (Value::Rational(a), Value::Rational(b)) => Value::Rational(a / b),
            (Value::Float(f), other) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(f / other)
            }
            (other, Value::Float(f)) => {
                let other: f64 = other.try_into().unwrap();
                Value::Float(other / f)
            }
            (a, b) => panic!("Division not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
            (Value::Bignum(a), Value::Integer(b)) => Value::Bignum(a % BigInt::from(b)),
            (a, b) => panic!("Remainder not implemented for {:?} and {:?}", a, b),
        }
    }
}

// impl Rem<isize> for Value {
//     type Output = isize;

//     fn rem(self, other: isize) -> isize {
//         match (self, other) {
//             (Value::Integer(a), b) => a % b,
//             (Value::Bignum(a), b) => a % b,
//             (a, b) => panic!("Remainder not implemented for {:?} and {:?}", a, b),
//         }
//     }
// }

pub trait IntegerDiv<RHS = Self> {
    type Output: Sized;

    fn int_div(self, other: RHS) -> Self::Output;
}

impl IntegerDiv for Value {
    type Output = Value;

    fn int_div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a / b),
            (Value::Bignum(a), Value::Integer(b)) => Value::Bignum(a / b),
            (a, b) => panic!("Integer Division not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Value {
    fn make_list(elems: &mut [Self]) -> Self {
        let mut res = Value::Nil;
        for next in elems.iter_mut().rev() {
            let pair = Pair(next.take(), res);
            res = Value::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    fn make_list_from_vec(elems: Vec<Self>) -> Self {
        let mut res = Value::Nil;
        for mut next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Value::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    fn make_vector(elems: &mut [Self]) -> Self {
        Value::Vector(Rc::new(RefCell::new(elems.to_vec())))
    }

    fn make_vector_from_vec(elems: Vec<Self>) -> Self {
        Value::Vector(Rc::new(RefCell::new(elems)))
    }

    fn make_dotted_list_from_vec(elems: Vec<Self>, tail: Self) -> Self {
        let mut res = tail;
        for mut next in elems.into_iter().rev() {
            let pair = Pair(next.take(), res);
            res = Value::Pair(Rc::new(RefCell::new(pair)));
        }
        res
    }

    fn make_pair(fst: Self, rst: Self) -> Self {
        let pair = Pair(fst, rst);
        Value::Pair(Rc::new(RefCell::new(pair)))
    }

    fn is_pair(&self) -> bool {
        match *self {
            Value::Pair(_) => true,
            _ => false,
        }
    }

    fn is_nil(&self) -> bool {
        *self == Value::Nil
    }

    fn take(&mut self) -> Self {
        mem::replace(self, Value::Undefined)
    }

    fn as_pair(&self) -> Result<Ref<Pair>, LispErr> {
        match *self {
            Value::Pair(ref ptr) => Ok(ptr.borrow()),
            ref other => Err(LispErr::TypeError("convert", "pair", other.clone())),
        }
    }

    fn as_mut_pair(&self) -> Result<RefMut<Pair>, LispErr> {
        match *self {
            Value::Pair(ref ptr) => Ok(ptr.borrow_mut()),
            ref other => Err(LispErr::TypeError("convert", "pair", other.clone())),
        }
    }

    fn as_vector(&self) -> Result<Ref<Vector>, LispErr> {
        match *self {
            Value::Vector(ref ptr) => Ok(ptr.borrow()),
            ref other => Err(LispErr::TypeError("convert", "vector", other.clone())),
        }
    }

    fn as_mut_vector(&self) -> Result<RefMut<Vector>, LispErr> {
        match *self {
            Value::Vector(ref ptr) => Ok(ptr.borrow_mut()),
            ref other => Err(LispErr::TypeError("convert", "vector", other.clone())),
        }
    }

    fn is_false(&self) -> bool {
        match *self {
            Value::Nil | Value::Bool(false) => true,
            _ => false,
        }
    }

    fn is_true(&self) -> bool {
        match *self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    // TODO: Better error handling
    // TODO: Distinction between `=`, `eq?`, `eqv?` and `equal?`
    fn compare(&self, other: &Self) -> Result<Ordering, LispErr> {
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
            (a, b) => panic!("Can't compare {:?} and {:?}", a, b),
        }
    }

    // TODO: Better error handling
    // TODO: Add vector equality
    fn is_equal(&self, other: &Self) -> Result<bool, LispErr> {
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

    fn to_string(&self, symbol_table: &symbol_table::SymbolTable) -> String {
        match *self {
            Value::Symbol(x) => symbol_table.lookup(x),
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Char(c) => format!("#\\{}", c),
            Value::Pair(ref ptr) => {
                let pair = ptr.borrow();
                let elems = pair.collect();
                let head = &elems[..(elems.len() - 1)];
                let tail = &elems[elems.len() - 1];

                let mut result = String::new();
                for (i, e) in head.iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table));
                }

                match tail {
                    &Value::Nil => (),
                    other => {
                        result.push_str(" . ");
                        result.push_str(&other.to_string(symbol_table));
                    }
                }

                format!("({})", result)
            }
            Value::Vector(ref elems) => {
                let mut result = String::new();
                for (i, e) in elems.borrow().iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table));
                }
                format!("#({})", result)
            }
            Value::ActivationFrame(ref elems) => {
                let mut result = String::new();
                for (i, e) in elems.iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table));
                }
                format!("#AF({})", result)
            }
            Value::Integer(x) => format!("{}", x),
            Value::Rational(ref x) => format!("{}", x),
            Value::Bignum(ref x) => format!("{}", x),
            Value::Float(x) => format!("{}", x),
            Value::String(ref s) => format!("\"{}\"", s),
            Value::Nil => "'()".to_string(),
            Value::Undefined => "undefined".to_string(),
            Value::Builtin(_, _, _) => "<builtin>".to_string(),
            Value::Closure(index, _, _, _) => format!("<closure {}>", index),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = LispErr;

    fn try_from(datum: Value) -> Result<String, LispErr> {
        match datum {
            Value::String(n) => Ok(n),
            other => Err(LispErr::TypeError("convert", "string", other)),
        }
    }
}

impl TryFrom<Value> for Fsize {
    type Error = LispErr;

    fn try_from(datum: Value) -> Result<Fsize, LispErr> {
        match datum {
            Value::Integer(n) => Ok(n as Fsize),
            Value::Rational(r) => Ok((*r.numer() as Fsize) / (*r.denom() as Fsize)),
            Value::Float(r) => Ok(r),
            other => Err(LispErr::TypeError("convert", "float", other)),
        }
    }
}
impl TryFrom<&Value> for Fsize {
    type Error = LispErr;

    fn try_from(datum: &Value) -> Result<Fsize, LispErr> {
        match datum {
            &Value::Integer(n) => Ok(n as Fsize),
            &Value::Rational(ref r) => Ok((*r.numer() as Fsize) / (*r.denom() as Fsize)),
            &Value::Float(r) => Ok(r),
            other => Err(LispErr::TypeError("convert", "float", other.clone())),
        }
    }
}

impl TryFrom<Value> for isize {
    type Error = LispErr;

    fn try_from(datum: Value) -> Result<isize, LispErr> {
        match datum {
            Value::Integer(n) => Ok(n),
            other => Err(LispErr::TypeError("convert", "integer", other)),
        }
    }
}
impl TryFrom<&Value> for isize {
    type Error = LispErr;

    fn try_from(datum: &Value) -> Result<isize, LispErr> {
        match datum {
            &Value::Integer(n) => Ok(n),
            other => Err(LispErr::TypeError("convert", "integer", other.clone())),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = LispErr;

    fn try_from(datum: Value) -> Result<usize, LispErr> {
        match datum {
            Value::Integer(n) if n >= 0 => Ok(n as usize),
            other => Err(LispErr::TypeError("convert", "uinteger", other)),
        }
    }
}
impl TryFrom<&Value> for usize {
    type Error = LispErr;

    fn try_from(datum: &Value) -> Result<usize, LispErr> {
        match datum {
            &Value::Integer(n) if n >= 0 => Ok(n as usize),
            other => Err(LispErr::TypeError("convert", "uinteger", other.clone())),
        }
    }
}

impl TryFrom<Value> for char {
    type Error = LispErr;

    fn try_from(datum: Value) -> Result<char, LispErr> {
        match datum {
            Value::Char(c) => Ok(c),
            other => Err(LispErr::TypeError("convert", "char", other)),
        }
    }
}
impl TryFrom<&Value> for char {
    type Error = LispErr;

    fn try_from(datum: &Value) -> Result<char, LispErr> {
        match datum {
            &Value::Char(c) => Ok(c),
            other => Err(LispErr::TypeError("convert", "char", other.clone())),
        }
    }
}

pub type Symbol = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);
