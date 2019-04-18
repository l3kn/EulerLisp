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
mod value;
mod vm;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use num::Rational;

use crate::symbol_table::SymbolTable;
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

pub type Symbol = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct BindingRef(usize, usize);

pub trait IntegerDiv<RHS = Self> {
    type Output: Sized;

    fn int_div(self, other: RHS) -> Self::Output;
}
