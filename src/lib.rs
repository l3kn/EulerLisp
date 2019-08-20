#![feature(try_trait)]

#[macro_use]
extern crate lisp_macros;

#[macro_use]
extern crate lazy_static;

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
pub mod value;

mod builtin;
mod env;
mod instruction;
mod lexer;
mod syntax_rule;
mod vm;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use crate::value::Value;
use crate::vm::VM;

pub type Fsize = f64;
pub type LispResult<T> = Result<T, LispError>;

impl From<std::option::NoneError> for LispError {
    fn from(error: std::option::NoneError) -> Self {
        LispError::NoneError
    }
}

#[derive(Debug)]
pub enum LispError {
    InvalidNumberOfArguments,
    InvalidList,
    InvalidTypeOfArguments,
    IndexOutOfBounds,
    DefinitionAlreadyDefined,
    DefinitionNotFound,
    IOError,
    NoneError,
    CompilerError(compiler::CompilerError),
    VMError(vm::VMError),
    LexerError(lexer::LexerError),
    ParserError(parser::ParserError),
    FormatterError(code_formatter::FormatterError),
    TypeError(&'static str, &'static str, Value),
}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LispError::InvalidNumberOfArguments => write!(f, "Invalid number of arguments"),
            LispError::InvalidList => write!(f, "Invalid list"),
            LispError::InvalidTypeOfArguments => write!(f, "Invalid types of arguments"),
            LispError::IndexOutOfBounds => write!(f, "Index out of bounds"),
            LispError::DefinitionNotFound => write!(f, "Definition not found"),
            LispError::DefinitionAlreadyDefined => write!(f, "Definition is already defined"),
            LispError::IOError => write!(f, "IO Error"),
            LispError::NoneError => write!(f, "None Error"),
            LispError::CompilerError(ref e) => write!(f, "Compiler Error, {}", e),
            LispError::VMError(ref e) => write!(f, "VM Error, {}", e),
            LispError::LexerError(ref e) => write!(f, "Lexer Error, {:?}", e),
            LispError::ParserError(ref e) => write!(f, "Parser Error, {:?}", e),
            LispError::FormatterError(ref e) => write!(f, "Formatter Error, {:?}", e),
            LispError::TypeError(fun, expected, ref got) => write!(
                f,
                "Type error evaluating {}: expected {}, got {}",
                fun, expected, got
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Pair(pub Value, pub Value);

impl Pair {
    pub fn compare(&self, other: &Pair) -> LispResult<Ordering> {
        let res1 = self.0.compare(&other.0)?;
        if res1 == Ordering::Equal {
            self.1.compare(&other.1)
        } else {
            Ok(res1)
        }
    }

    pub fn is_equal(&self, other: &Pair) -> LispResult<bool> {
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

    pub fn collect_list(&self) -> LispResult<Vec<Value>> {
        let mut v = self.collect();
        let last = v.pop().unwrap();

        if Value::Nil == last {
            Ok(v)
        } else {
            Err(LispError::InvalidList)
        }
    }
}

pub type PairRef = Rc<RefCell<Pair>>;

pub type Vector = Vec<Value>;
pub type VectorRef = Rc<RefCell<Vector>>;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);
