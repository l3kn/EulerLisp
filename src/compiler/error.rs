use std::fmt;

use crate::symbol_table::Symbol;
use crate::{LispError, Value};

#[derive(PartialEq)]
pub enum CompilerError {
    UndefinedVariable(Symbol),
    ReservedName(Symbol),
    NonSelfEvaluatingConstant(Symbol),
    ConstantReassignment(Symbol),
    NoMatchingMacroPattern(Value),
    InvalidFunctionArgument(Value),
    IncorrectPrimitiveArity(Symbol, usize, usize),
    InvalidInternalDefinition,
}

impl fmt::Debug for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompilerError::UndefinedVariable(ref v) => write!(f, "Undefined variable {}", v),
            CompilerError::ReservedName(ref v) => write!(f, "{} is a reserved name", v),
            CompilerError::NonSelfEvaluatingConstant(ref v) => {
                write!(f, "constant {} is not self-evaluating", v)
            }
            CompilerError::NoMatchingMacroPattern(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "no matching macro pattern __")
            }
            CompilerError::InvalidFunctionArgument(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "__ is not a valid function argument")
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

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompilerError::UndefinedVariable(ref v) => write!(f, "Undefined variable {}", v),
            CompilerError::ReservedName(ref v) => write!(f, "{} is a reserved name", v),
            CompilerError::NonSelfEvaluatingConstant(ref v) => {
                write!(f, "constant {} is not self-evaluating", v)
            }
            CompilerError::NoMatchingMacroPattern(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "no matching macro pattern __")
            }
            CompilerError::InvalidFunctionArgument(ref v) => {
                // TODO: Error Printing m symbol table
                write!(f, "__ is not a valid function argument")
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

impl From<CompilerError> for LispError {
    fn from(error: CompilerError) -> Self {
        LispError::CompilerError(error)
    }
}
