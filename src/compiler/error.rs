use std::fmt;

use crate::{Expression, LispErr};

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
