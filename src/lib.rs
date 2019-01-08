#[macro_use]
mod macros;

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
mod heap;

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::{Sub, Neg, Div, Rem};
use num::{Rational, BigInt};

use crate::symbol_table::SymbolTable;
use crate::heap::{Heap, PairRef, VectorRef, EnvRef, ActivationFrameRef, StringRef, BignumRef};

pub type Fsize = f64;
pub type LispResult<T> = Result<T, LispErr>;

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
    TypeError(&'static str, &'static str, Datum),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
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

pub type LispFn1 = fn(Datum, &vm::OutputRef, &mut SymbolTable, &mut Heap) -> LispResult<Datum>;
pub type LispFn2 = fn(Datum, Datum, &vm::OutputRef, &mut SymbolTable, &mut Heap) -> LispResult<Datum>;
pub type LispFn3 = fn(Datum, Datum, Datum, &vm::OutputRef, &mut SymbolTable, &mut Heap) -> LispResult<Datum>;
pub type LispFnN = fn(&mut [Datum], &vm::OutputRef, &mut SymbolTable, &mut Heap) -> LispResult<Datum>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum LispFnType { Fixed1, Fixed2, Fixed3, Variadic }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pair(pub Datum, pub Datum);

impl Default for Pair {
    fn default() -> Pair {
        Pair(Datum::Undefined, Datum::Undefined)
    }
}

pub type Vector = Vec<Datum>;
pub type ActivationFrame = Vec<Datum>;

#[derive(Clone, Debug, Copy)]
pub enum Datum {
    Bool(bool),
    Integer(isize),
    Rational(Rational),
    Float(Fsize),
    Bignum(BignumRef),
    Char(char),
    String(StringRef),
    Symbol(Symbol),
    Pair(PairRef),
    Vector(VectorRef),
    Builtin(LispFnType, u16, Arity),
    ActivationFrame(ActivationFrameRef),
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
    // FIXME: The compiler adds builtins as constants sometimes.
    // I don't want to move the `Heap` into the compiler,
    // but if it were to use `Datums` for constants,
    // it would need some way to access the heap.
    Builtin(LispFnType, u16, Arity),
}

impl Expression {
    fn to_datum(self, st: &mut SymbolTable, heap: &mut Heap) -> Datum {
        match self {
            Expression::Bool(v) => Datum::Bool(v),
            Expression::Integer(v) => Datum::Integer(v),
            Expression::Float(v) => Datum::Float(v),
            Expression::Rational(r) => Datum::Rational(r),
            Expression::Char(v) => Datum::Char(v),
            Expression::String(v) => heap.make_string(v),
            Expression::Symbol(v) => Datum::Symbol(st.insert(&v)),
            Expression::List(es) => {
                let ds = es.into_iter().map(|e| e.to_datum(st, heap)).collect();
                heap.make_list_from_vec(ds)
            }
            Expression::DottedList(es, tail) => {
                let ds = es.into_iter().map(|e| e.to_datum(st, heap)).collect();
                let tail = tail.to_datum(st, heap);
                heap.make_dotted_list_from_vec(ds, tail)
            }
            Expression::Vector(es) => {
                let ds = es.into_iter().map(|e| e.to_datum(st, heap)).collect();
                heap.make_vector(ds)
            }
            Expression::Undefined => Datum::Undefined,
            Expression::Nil => Datum::Nil,
            Expression::Builtin(typ, idx, arity) => Datum::Builtin(typ, idx, arity),
        }
    }

    pub fn is_self_evaluating(&self) -> bool {
        use self::Expression::*;
        match *self {
            Symbol(_) | Char(_) |
            Vector(_) | Integer(_) |
            Rational(_) | Float(_) |
            String(_) |
            Nil | Undefined => true,
            _ => false,
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

fn within_epsilon(f1: f64, f2: f64) -> bool {
    (f1 - f2).abs() < std::f64::EPSILON
}

impl PartialEq for Expression {
    fn eq(&self, other: &Expression) -> bool {
        match (self, other) {
            (&Expression::Bool(a), &Expression::Bool(b)) => a == b,
            (&Expression::Char(a), &Expression::Char(b)) => a == b,
            (&Expression::Symbol(ref a), &Expression::Symbol(ref b)) => a == b,
            (&Expression::String(ref a), &Expression::String(ref b)) => a == b,
            (&Expression::Integer(a), &Expression::Integer(b)) => a == b,
            (&Expression::Float(a), &Expression::Float(b)) => within_epsilon(a, b),
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
            Expression::Builtin(_, _, _) => write!(f, "<builtin>"),
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
            (&Datum::Float(a), &Datum::Float(b)) => within_epsilon(a, b),
            (&Datum::Pair(ref a1), &Datum::Pair(ref b1)) => a1 == b1,
            (&Datum::Vector(ref a), &Datum::Vector(ref b)) => a == b,
            (&Datum::Undefined, &Datum::Undefined) => true,
            (&Datum::Nil, &Datum::Nil) => true,
            _ => false,
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
            }
            Datum::Symbol(v) => v.hash(state),
            Datum::Pair(ptr) => {
                "pair".hash(state);
                ptr.hash(state);
            }
            Datum::ActivationFrame(ptr) => {
                "activation".hash(state);
                ptr.hash(state);
            }
            Datum::Vector(ptr) => {
                "vector".hash(state);
                ptr.hash(state);
            }
            Datum::Undefined => {
                "undefined".hash(state);
            }
            Datum::Nil => {
                "nil".hash(state);
            }
            Datum::Builtin(ref typ, index, ref arity) => {
                "builtin".hash(state);
                typ.hash(state);
                index.hash(state);
                arity.hash(state);
            }
            Datum::Float(v) => {
                // This is pretty bit hacky but better than not allowing floats
                // to be used as hash keys.
                // This eq is only meant to be used for hashmaps,
                // so it's not that bad.
                "float".hash(state);
                v.to_string().hash(state);
            }
            Datum::Closure(offset, _, _, _) => {
                "closure".hash(state);
                offset.hash(state);
            }
        }
    }
}

impl Sub for Datum {
    type Output = Datum;

    fn sub(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => Datum::Integer(a - b),
            // FIXME
            // (Datum::Integer(a), Datum::Bignum(b)) => Datum::Bignum(BigInt::from(a) - b),
            // (Datum::Bignum(a), Datum::Integer(b)) => Datum::Bignum(a - BigInt::from(b)),
            // (Datum::Bignum(a), Datum::Bignum(b)) => Datum::Bignum(a - b),
            (Datum::Rational(a), Datum::Integer(b)) => Datum::Rational(a - b),
            // `+ -a` because only `rational + isize` and `rational - isize` are supported
            (Datum::Integer(a), Datum::Rational(b)) => Datum::Rational(b + -a),
            (Datum::Rational(a), Datum::Rational(b)) => Datum::Rational(a - b),
            (Datum::Float(f), other) => Datum::Float(f - other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(other.as_float().unwrap() - f),
            (a, b) => panic!("Subtraction not implemented for {:?} and {:?}", a, b),
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
            a => panic!("Negation not implemented for {:?}", a),
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
            }
            (Datum::Integer(a), Datum::Rational(b)) => Datum::Rational(Rational::from(a) / b),
            (Datum::Rational(a), Datum::Integer(b)) => Datum::Rational(a / b),
            (Datum::Rational(a), Datum::Rational(b)) => Datum::Rational(a / b),
            (Datum::Float(f), other) => Datum::Float(f / other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(other.as_float().unwrap() / f),
            (a, b) => panic!("Division not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Rem for Datum {
    type Output = Datum;

    fn rem(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => Datum::Integer(a % b),
            // FIXME
            // (Datum::Bignum(a), Datum::Integer(b)) => Datum::Bignum(a % BigInt::from(b)),
            (a, b) => panic!("Remainder not implemented for {:?} and {:?}", a, b),
        }
    }
}

// impl Rem<isize> for Datum {
//     type Output = isize;

//     fn rem(self, other: isize) -> isize {
//         match (self, other) {
//             (Datum::Integer(a), b) => a % b,
//             (Datum::Bignum(a), b) => a % b,
//             (a, b) => panic!("Remainder not implemented for {:?} and {:?}", a, b),
//         }
//     }
// }

pub trait IntegerDiv<RHS = Self> {
    type Output: Sized;

    fn int_div(self, other: RHS) -> Self::Output;
}

impl IntegerDiv for Datum {
    type Output = Datum;

    fn int_div(self, other: Datum) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => Datum::Integer(a / b),
            // FIXME
            // (Datum::Bignum(a), Datum::Integer(b)) => Datum::Bignum(a / b),
            (a, b) => panic!("Integer Division not implemented for {:?} and {:?}", a, b),
        }
    }
}

impl Datum {
    fn add(self, other: Datum, heap: &mut Heap) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => match a.checked_add(b) {
                Some(r) => Datum::Integer(r),
                None => heap.make_bignum(BigInt::from(a) * BigInt::from(b))
            },
            (Datum::Integer(a), Datum::Bignum(b)) => heap.make_bignum(
                BigInt::from(a) + heap.get_bignum(b)
            ),
            (Datum::Bignum(a), Datum::Integer(b)) => heap.make_bignum(
                heap.get_bignum(a) + BigInt::from(b)
            ),
            (Datum::Bignum(a), Datum::Bignum(b)) => heap.make_bignum(
                heap.get_bignum(a) + heap.get_bignum(b)
            ),
            (Datum::Rational(a), Datum::Integer(b)) => Datum::Rational(a + b),
            (Datum::Integer(a), Datum::Rational(b)) => Datum::Rational(b + a),
            (Datum::Rational(a), Datum::Rational(b)) => Datum::Rational(a + b),
            (Datum::Float(f), other) => Datum::Float(f + other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(f + other.as_float().unwrap()),
            (a, b) => panic!("Addition not implemented for {:?} and {:?}", a, b),
        }
    }

    fn mult(self, other: Datum, heap: &mut Heap) -> Datum {
        match (self, other) {
            (Datum::Integer(a), Datum::Integer(b)) => match a.checked_mul(b) {
                Some(r) => Datum::Integer(r),
                None => heap.make_bignum(BigInt::from(a) * BigInt::from(b))
            },
            (Datum::Integer(a), Datum::Rational(b)) => Datum::Rational(b * a),
            (Datum::Integer(a), Datum::Bignum(b)) => heap.make_bignum(
                BigInt::from(a) * heap.get_bignum(b)
            ),
            (Datum::Bignum(a), Datum::Integer(b)) => heap.make_bignum(
                heap.get_bignum(a) * BigInt::from(b)
            ),
            (Datum::Bignum(a), Datum::Bignum(b)) => heap.make_bignum(
                heap.get_bignum(a) * heap.get_bignum(b)
            ),
            (Datum::Rational(a), Datum::Integer(b)) => Datum::Rational(a * b),
            (Datum::Rational(a), Datum::Rational(b)) => Datum::Rational(a * b),
            (Datum::Float(f), other) => Datum::Float(f * other.as_float().unwrap()),
            (other, Datum::Float(f)) => Datum::Float(f * other.as_float().unwrap()),
            (a, b) => panic!("Multiplication not implemented for {:?} and {:?}", a, b),
        }
    }

    fn is_pair(&self) -> bool {
        match *self {
            Datum::Pair(_) => true,
            _ => false,
        }
    }

    fn is_nil(&self) -> bool {
        *self == Datum::Nil
    }

    fn take(&mut self) -> Self {
        mem::replace(self, Datum::Undefined)
    }

    fn as_float(&self) -> Result<Fsize, LispErr> {
        match *self {
            Datum::Integer(n) => Ok(n as Fsize),
            Datum::Rational(ref r) => Ok((*r.numer() as Fsize) / (*r.denom() as Fsize)),
            Datum::Float(r) => Ok(r),
            ref other => Err(LispErr::TypeError("convert", "float", other.clone())),
        }
    }

    fn as_integer(&self) -> Result<isize, LispErr> {
        match *self {
            Datum::Integer(n) => Ok(n),
            ref other => Err(LispErr::TypeError("convert", "integer", other.clone())),
        }
    }

    fn as_uinteger(&self) -> Result<usize, LispErr> {
        match *self {
            Datum::Integer(n) => {
                if n >= 0 {
                    Ok(n as usize)
                } else {
                    Err(LispErr::TypeError("convert", "uinteger", self.clone()))
                }
            }
            ref other => Err(LispErr::TypeError("convert", "uinteger", other.clone())),
        }
    }

    fn as_char(&self) -> Result<char, LispErr> {
        match *self {
            Datum::Char(n) => Ok(n),
            ref other => Err(LispErr::TypeError("convert", "char", other.clone())),
        }
    }

    fn is_false(&self) -> bool {
        match *self {
            Datum::Nil | Datum::Bool(false) => true,
            _ => false,
        }
    }

    fn is_true(&self) -> bool {
        match *self {
            Datum::Nil | Datum::Bool(false) => false,
            _ => true,
        }
    }

    // TODO: Better error handling
    // TODO: Distinction between `=`, `eq?`, `eqv?` and `equal?`
    fn compare(&self, other: &Self, heap: &Heap) -> Result<Ordering, LispErr> {
        use self::Datum::*;
        match (self, other) {
            (&Integer(ref a), &Integer(ref b)) => Ok(a.cmp(b)),
            (&Symbol(ref a), &Symbol(ref b)) => Ok(a.cmp(b)),
            (&Bignum(a), &Bignum(b)) => {
                let ba = heap.get_bignum(a);
                let bb = heap.get_bignum(b);
                Ok(ba.cmp(bb))
            }
            (&Integer(a), &Bignum(b)) => {
                let ba = BigInt::from(a);
                let bb = heap.get_bignum(b);
                Ok(ba.cmp(bb))
            }
            (&Bignum(a), &Integer(b)) => {
                let ba = heap.get_bignum(a);
                let bb = BigInt::from(b);
                Ok(ba.cmp(&bb))
            }
            (&Rational(ref a), &Rational(ref b)) => {
                Ok(a.cmp(&b))
            }
            // TODO: The two below can overflow
            (&Integer(ref a), &Rational(ref b)) => Ok((a * b.denom()).cmp(&(b.numer()))),
            (&Rational(ref a), &Integer(ref b)) => Ok(a.numer().cmp(&(b * a.denom()))),
            (ref other, &Float(ref b)) => Ok((other.as_float()?).partial_cmp(b).unwrap()),
            (&Float(ref b), ref other) => Ok(b.partial_cmp(&other.as_float()?).unwrap()),
            (&String(a), &String(b)) => {
                let sa = heap.get_string(a);
                let sb = heap.get_string(b);
                Ok(sa.cmp(sb))
            },
            (&Char(ref a), &Char(ref b)) => Ok(a.cmp(b)),
            (&Pair(a), &Pair(b)) => {
                let pa = heap.get_pair(a);
                let pb = heap.get_pair(b);
                let c0 = pa.0.compare(&pb.0, heap);

                if let Ok(Ordering::Equal) = c0 {
                    pa.1.compare(&pb.1, heap)
                } else {
                    c0
                }
            },
            (&Nil, &Nil) => Ok(Ordering::Equal),
            (a, b) => panic!("Can't compare {:?} and {:?}", a, b),
        }
    }

    // TODO: Better error handling
    // TODO: Add vector equality
    // TODO: Different equality predicates like scheme,
    // `eq?` for `ptr` equality,
    // `equal?` for value equality
    fn is_equal(&self, other: &Self, heap: &Heap) -> Result<bool, LispErr> {
        use self::Datum::*;
        match (self, other) {
            (&Integer(ref a), &Integer(ref b)) => Ok(a == b),
            (&Symbol(ref a), &Symbol(ref b)) => Ok(a == b),
            (&Bignum(a), &Bignum(b)) => {
                let ba = heap.get_bignum(a);
                let bb = heap.get_bignum(b);
                Ok(ba == bb)
            }
            (&Integer(a), &Bignum(b)) => {
                let ba = BigInt::from(a);
                let bb = heap.get_bignum(b);
                Ok(ba == *bb)
            }
            (&Bignum(a), &Integer(b)) => {
                let ba = heap.get_bignum(a);
                let bb = BigInt::from(b);
                Ok(*ba == bb)
            }
            (&Rational(ref a), &Rational(ref b)) => {
                Ok(a == b)
            }
            (&Integer(ref a), &Rational(ref b)) => Ok((a * b.denom()) == *b.numer()),
            (&Rational(ref a), &Integer(ref b)) => Ok(*a.numer() == (b * a.denom())),
            (ref other, &Float(b)) => Ok(within_epsilon(other.as_float()?, b)),
            (&Float(b), ref other) => Ok(within_epsilon(b, other.as_float()?)),
            (&String(a), &String(b)) => {
                let sa = heap.get_string(a);
                let sb = heap.get_string(b);
                Ok(sa == sb)
            },
            (&Char(a), &Char(b)) => Ok(a == b),
            (&Pair(a), &Pair(b)) => {
                let pa = heap.get_pair(a);
                let pb = heap.get_pair(b);
                if pa.0.is_equal(&pb.0, heap)? {
                    pa.1.is_equal(&pb.1, heap)
                } else {
                    Ok(false)
                }
            },
            (&Nil, &Nil) => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn is_self_evaluating(&self) -> bool {
        use self::Datum::*;
        match *self {
            Symbol(_) | Char(_) |
            Vector(_) | Integer(_) |
            Rational(_) | Float(_) |
            Bignum(_) | String(_) |
            Nil | Undefined => true,
            _ => false,
        }
    }

    fn to_string(&self,
        symbol_table: &symbol_table::SymbolTable,
        heap: &heap::Heap,
        // Depending on where we want to use the string,
        // Datum::String("foo") should return "\"foo\"" or just "foo"
        //
        // The pretty version is used for the repl and the debugger.
        pretty: bool,
    ) -> String {
        match *self {
            Datum::Symbol(x) => symbol_table.lookup(x),
            Datum::Bool(true) => "#t".to_string(),
            Datum::Bool(false) => "#f".to_string(),
            Datum::Char(c) => format!("#\\{}", c),
            Datum::Pair(ptr) => {
                // FIXME
                let elems = heap.get_pair_dotted_list(ptr);
                let head = &elems[..(elems.len() - 1)];
                let tail = &elems[elems.len() - 1];

                let mut result = String::new();
                for (i, e) in head.iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table, heap, true));
                }

                match tail {
                    &Datum::Nil => (),
                    other => {
                        result.push_str(" . ");
                        result.push_str(&other.to_string(symbol_table, heap, true));
                    }
                }

                format!("({})", result)
            }
            Datum::Vector(ptr) => {
                let mut result = String::new();
                for (i, e) in heap.get_vector(ptr).iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table, heap, true));
                }
                format!("#({})", result)
            }
            Datum::ActivationFrame(ptr) => {
                let mut result = String::new();
                for (i, e) in heap.get_vector(ptr).iter().enumerate() {
                    if i != 0 {
                        result.push_str(" ");
                    }
                    result.push_str(&e.to_string(symbol_table, heap, pretty));
                }
                format!("#AF({})", result)
            }
            Datum::Integer(x) => format!("{}", x),
            Datum::Rational(ref x) => format!("{}", x),
            Datum::Bignum(ref x) => format!("{}", x),
            Datum::Float(x) => format!("{}", x),
            Datum::String(ptr) => {
                let s = heap.get_string(ptr);
                if pretty {
                    format!("\"{}\"", s)
                } else {
                    s.to_string()
                }
            },
            Datum::Nil => "'()".to_string(),
            Datum::Undefined => "undefined".to_string(),
            Datum::Builtin(_, _, _) => "<builtin>".to_string(),
            Datum::Closure(index, _, _, _) => format!("<closure {}>", index),
        }
    }
}

pub type Symbol = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);
