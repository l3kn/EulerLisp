use std::fmt;

use crate::symbol_table::Symbol;
use crate::{LispError, Value};

#[derive(PartialEq)]
pub enum Error {
    UndefinedVariable(Symbol),
    ReservedName(Symbol),
    NonSelfEvaluatingConstant(Symbol),
    ConstantReassignment(Symbol),
    NoMatchingMacroPattern(Value),
    InvalidFunctionArgument(Value),
    IncorrectPrimitiveArity(Symbol, usize, usize),
    InvalidInternalDefinition,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UndefinedVariable(ref v) => write!(f, "Undefined variable {}", v),
            Error::ReservedName(ref v) => write!(f, "{} is a reserved name", v),
            Error::NonSelfEvaluatingConstant(ref v) => {
                write!(f, "constant {} is not self-evaluating", v)
            }
            Error::NoMatchingMacroPattern(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "no matching macro pattern __")
            }
            Error::InvalidFunctionArgument(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "__ is not a valid function argument")
            }
            Error::ConstantReassignment(ref v) => write!(f, "can not reassign the constant {}", v),
            Error::IncorrectPrimitiveArity(ref v, e, g) => write!(
                f,
                "incorrect arity for primitive {}, expected {}, got {}",
                v, e, g
            ),
            Error::InvalidInternalDefinition => write!(
                f,
                "internal definition must appear at the beginning of the body"
            ),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UndefinedVariable(ref v) => write!(f, "Undefined variable {}", v),
            Error::ReservedName(ref v) => write!(f, "{} is a reserved name", v),
            Error::NonSelfEvaluatingConstant(ref v) => {
                write!(f, "constant {} is not self-evaluating", v)
            }
            Error::NoMatchingMacroPattern(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "no matching macro pattern __")
            }
            Error::InvalidFunctionArgument(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "__ is not a valid function argument")
            }
            Error::ConstantReassignment(ref v) => write!(f, "can not reassign the constant {}", v),
            Error::IncorrectPrimitiveArity(ref v, e, g) => write!(
                f,
                "incorrect arity for primitive {}, expected {}, got {}",
                v, e, g
            ),
            Error::InvalidInternalDefinition => write!(
                f,
                "internal definition must appear at the beginning of the body"
            ),
        }
    }
}

impl From<Error> for LispError {
    fn from(error: Error) -> Self {
        LispError::CompilerError(error)
    }
}
