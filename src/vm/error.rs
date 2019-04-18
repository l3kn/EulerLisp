use std::fmt;

pub enum Error {
    EnvStackUnderflow(usize),
    PCStackUnderflow(usize),
    StackUnderflow(usize),
    InstructionFetchError,
}

pub type Result = std::result::Result<(), Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::EnvStackUnderflow(inst) => write!(f, "Environment Stack Underflow @ {}", inst),
            Error::PCStackUnderflow(inst) => write!(f, "PC Stack Underflow @ {}", inst),
            Error::StackUnderflow(inst) => write!(f, "Stack Underflow @ {}", inst),
            Error::InstructionFetchError => write!(f, "Instruction fetch error"),
        }
    }
}
