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
pub mod parser;
pub mod repl;
pub mod symbol_table;
pub mod value;
pub mod vm;

mod builtin;
mod env;
mod instruction;
mod lexer;
mod syntax_rule;

use std::cell::{Cell, Ref, RefCell};
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
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
    CompilerError(compiler::Error),
    VMError(vm::Error),
    LexerError(lexer::Error),
    ParserError(parser::Error),
    FormatterError(code_formatter::Error),
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

pub struct Pair {
    fst: RefCell<Value>,
    rst: RefCell<Value>,
}

impl Pair {
    pub fn new(fst: Value, rst: Value) -> Self {
        Self {
            fst: RefCell::new(fst),
            rst: RefCell::new(rst),
        }
    }

    pub fn get_fst(&self) -> Ref<Value> {
        self.fst.borrow()
    }

    // TODO: Do this without mutability
    pub fn get_rst(&self) -> Ref<Value> {
        self.rst.borrow()
    }

    pub fn set_fst(&self, value: Value) {
        self.fst.replace(value);
    }

    pub fn set_rst(&self, value: Value) {
        self.rst.replace(value);
    }

    pub fn compare(&self, other: &Pair) -> LispResult<Ordering> {
        let res1 = self.get_fst().compare(&other.get_fst())?;
        if res1 == Ordering::Equal {
            self.rst.borrow().compare(&other.rst.borrow())
        } else {
            Ok(res1)
        }
    }

    pub fn is_equal(&self, other: &Pair) -> LispResult<bool> {
        if !self.fst.borrow().is_equal(&other.fst.borrow())? {
            return Ok(false);
        }

        self.rst.borrow().is_equal(&other.rst.borrow())
    }

    fn collect_into(&self, mut res: Vec<Value>) -> Vec<Value> {
        res.push(self.fst.borrow().clone());
        self.rst.borrow().collect_into(res)
    }

    pub fn collect(&self) -> Vec<Value> {
        self.collect_into(vec![])
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

impl Hash for Pair {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fst.borrow().hash(state);
        self.rst.borrow().hash(state);
    }
}

pub type PairRef = Rc<Pair>;

pub type Vector = Vec<Value>;
pub type VectorRef = Rc<RefCell<Vector>>;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);
