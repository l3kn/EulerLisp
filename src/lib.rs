#[macro_use]
extern crate lisp_macros;

#[macro_use]
mod macros;

pub mod code_formatter;
pub mod compiler;
// pub mod debugger;
pub mod doc;
pub mod evaluator;
pub mod parser;
pub mod repl;
pub mod symbol_table;
pub mod value;

mod builtin;
mod env;
// mod expression;
mod instruction;
mod lexer;
mod syntax_rule;
mod vm;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use crate::compiler::CompilerError;
// use crate::expression::Expression;
use crate::value::Value;
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

pub type Symbol = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);

pub trait IntegerDiv<RHS = Self> {
    type Output: Sized;

    fn int_div(self, other: RHS) -> Self::Output;
}
