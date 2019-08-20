//! Instructions of the virtual machine

use std::collections::HashMap;

use byteorder::{LittleEndian, WriteBytesExt};

#[derive(Clone, Copy, Debug)]
#[repr(usize)]
pub enum Instruction {
    /// `VAL = VAL+1`
    Inc,
    /// `VAL = VAL+1`
    Dec,
    /// `VAL = VAL + ARG1`
    Add,
    /// `VAL = VAL - ARG1`
    Sub,
    /// `VAL = VAL * ARG1`
    Mul,
    /// `VAL = VAL / ARG1`
    Div,
    /// `VAL = VAL // ARG1`
    IntDiv,
    /// `VAL = VAL % ARG1`
    Mod,
    ///# Pairs
    /// First element of a pair, car
    /// `VAL = (fst VAL)`
    Fst,
    /// Second element of a pair, cdr
    /// `VAL = (rst VAL)`
    Rst,
    /// Pair construction
    /// `VAL = (cons VAL ARG1)`
    Cons,
    // Comparison
    Not,
    Equal,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    IsZero,
    IsNil,
    // Vector
    VectorRef,
    VectorSet,
    PushValue,
    // Stack
    PopArg1,
    PopArg2,
    PopFunction,
    // Function Invocation
    FunctionInvoke(bool, u8),
    PreserveEnv,
    RestoreEnv,
    ExtendEnv,
    UnlinkEnv,
    Constant(u16),
    PushConstant(u16),
    // Environment
    CheckedGlobalRef(u16),
    GlobalRef(u16),
    PushCheckedGlobalRef(u16),
    PushGlobalRef(u16),
    GlobalSet(u16),
    ShallowArgumentRef(u16),
    PushShallowArgumentRef(u16),
    ShallowArgumentSet(u16),
    DeepArgumentRef(u16, u16),
    PushDeepArgumentRef(u16, u16),
    DeepArgumentSet(u16, u16),
    // Jumps
    Jump(u32),
    JumpFalse(u32),
    JumpTrue(u32),
    JumpNil(u32),
    JumpNotNil(u32),
    JumpZero(u32),
    JumpNotZero(u32),
    // Closures
    FixClosure(u16),
    DottedClosure(u16),
    StoreArgument(u8),
    ConsArgument(u8),
    AllocateFrame(u8),
    AllocateFillFrame(u8),
    AllocateDottedFrame(u8),
    // Control
    Return,
    Finish,
    // Continuation
    CallCC,
}

// Create the correct variant of `write_...::<LittleEndian>()`
// for each of the used types
macro_rules! write_type {
    ($to:ident, $var:ident, u8) => {{
        $to.write_u8(*$var).unwrap();
    }};
    ($to:ident, $var:ident, u16) => {{
        $to.write_u16::<LittleEndian>(*$var).unwrap();
    }};
    ($to:ident, $var:ident, u32) => {{
        $to.write_u32::<LittleEndian>(*$var).unwrap();
    }};
}

// Encode an instruction by creating a vector with its instruction_byte
// and then writing all its parameters to this vector
macro_rules! encode_inst {
    ($instruction_byte:expr) => ({
        vec![$instruction_byte]
    });
    ($instruction_byte:expr, $($var:ident: $type:tt),*) => ({
        let mut res = vec![$instruction_byte];
        $(write_type!(res, $var, $type);)*
        res
    })
}

impl Instruction {
    pub fn encode(&self) -> Vec<u8> {
        use self::Instruction::*;

        match self {
            Return => vec![0x00_u8],
            Finish => vec![0x01_u8],

            Inc => vec![0x10_u8],
            Dec => vec![0x11_u8],
            Add => vec![0x12_u8],
            Sub => vec![0x13_u8],
            Mul => vec![0x14_u8],
            Div => vec![0x15_u8],
            Mod => vec![0x16_u8],
            IntDiv => vec![0x17_u8],

            Not => vec![0x18_u8],
            Equal => vec![0x19_u8],
            Eq => vec![0x1A_u8],
            Neq => vec![0x1B_u8],
            Gt => vec![0x1C_u8],
            Gte => vec![0x1D_u8],
            Lt => vec![0x1E_u8],
            Lte => vec![0x1F_u8],

            Fst => vec![0x20_u8],
            Rst => vec![0x21_u8],
            Cons => vec![0x22_u8],
            IsZero => vec![0x23_u8],
            IsNil => vec![0x24_u8],
            VectorRef => vec![0x25_u8],
            VectorSet => vec![0x26_u8],

            Constant(index) => encode_inst!(0x30_u8, index: u16),
            PushConstant(index) => encode_inst!(0x31_u8, index: u16),
            PushValue => vec![0x32_u8],
            PopFunction => vec![0x33_u8],
            PopArg1 => vec![0x34_u8],
            PopArg2 => vec![0x35_u8],
            PreserveEnv => vec![0x36_u8],
            RestoreEnv => vec![0x37_u8],
            ExtendEnv => vec![0x38_u8],
            UnlinkEnv => vec![0x39_u8],

            CheckedGlobalRef(index) => encode_inst!(0x40_u8, index: u16),
            GlobalRef(index) => encode_inst!(0x41_u8, index: u16),
            PushCheckedGlobalRef(index) => encode_inst!(0x42_u8, index: u16),
            PushGlobalRef(index) => encode_inst!(0x43_u8, index: u16),
            GlobalSet(index) => encode_inst!(0x44_u8, index: u16),
            ShallowArgumentRef(index) => encode_inst!(0x60_u8, index: u16),
            PushShallowArgumentRef(index) => encode_inst!(0x61_u8, index: u16),
            ShallowArgumentSet(index) => encode_inst!(0x62_u8, index: u16),
            DeepArgumentRef(i, j) => encode_inst!(0x63_u8, i: u16, j: u16),
            PushDeepArgumentRef(i, j) => encode_inst!(0x64_u8, i: u16, j: u16),
            DeepArgumentSet(i, j) => encode_inst!(0x65_u8, i: u16, j: u16),

            Jump(index) => encode_inst!(0x70_u8, index: u32),
            JumpTrue(index) => encode_inst!(0x71_u8, index: u32),
            JumpFalse(index) => encode_inst!(0x72_u8, index: u32),
            JumpNil(index) => encode_inst!(0x73_u8, index: u32),
            JumpNotNil(index) => encode_inst!(0x74_u8, index: u32),
            JumpZero(index) => encode_inst!(0x75_u8, index: u32),
            JumpNotZero(index) => encode_inst!(0x76_u8, index: u32),

            FixClosure(arity) => encode_inst!(0x80_u8, arity: u16),
            DottedClosure(arity) => encode_inst!(0x81_u8, arity: u16),
            StoreArgument(index) => encode_inst!(0x82_u8, index: u8),
            ConsArgument(index) => encode_inst!(0x83_u8, index: u8),
            AllocateFrame(index) => encode_inst!(0x84_u8, index: u8),
            AllocateFillFrame(index) => encode_inst!(0x85_u8, index: u8),
            AllocateDottedFrame(index) => encode_inst!(0x86_u8, index: u8),
            FunctionInvoke(false, arity) => encode_inst!(0x87, arity: u8),
            FunctionInvoke(true, arity) => encode_inst!(0x88, arity: u8),

            CallCC => vec![0x89],
        }
    }

    /// Size of each instruction in bytes
    pub fn size(&self) -> usize {
        use self::Instruction::*;

        match self {
            Return | Finish | Inc | Dec | Add | Sub | Mul | Div | Mod | IntDiv | Not | Equal
            | Eq | Neq | Gt | Gte | Lt | Lte | Fst | Rst | Cons | IsZero | IsNil | VectorRef
            | VectorSet => 1,
            Constant(_) | PushConstant(_) => 3,
            PushValue | PopFunction | PopArg1 | PopArg2 | PreserveEnv | RestoreEnv | ExtendEnv
            | UnlinkEnv => 1,

            CheckedGlobalRef(_) => 3,
            GlobalRef(_) => 3,
            PushCheckedGlobalRef(_) => 3,
            PushGlobalRef(_) => 3,
            GlobalSet(_) => 3,

            ShallowArgumentRef(_) => 3,
            PushShallowArgumentRef(_) => 3,
            ShallowArgumentSet(_) => 3,
            DeepArgumentRef(_, _) => 5,
            PushDeepArgumentRef(_, _) => 5,
            DeepArgumentSet(_, _) => 5,

            Jump(_) | JumpTrue(_) | JumpFalse(_) | JumpNil(_) | JumpNotNil(_) | JumpZero(_)
            | JumpNotZero(_) => 5,

            FixClosure(_) => 3,
            DottedClosure(_) => 3,
            StoreArgument(_) => 2,
            ConsArgument(_) => 2,
            AllocateFrame(_) => 2,
            AllocateFillFrame(_) => 2,
            AllocateDottedFrame(_) => 2,
            FunctionInvoke(_, _) => 2,

            CallCC => 1,
        }
    }
}

pub type LabeledInstruction = (Instruction, Option<usize>);

/// Convert a list of `LabeledInstruction` to a list of bytes
/// and rewrite labeled jumps to relative jumps
///
/// Because the optimization pass can remove instructions
/// all the jumps need to be updated.
/// To do so, all the compiler functions produce `LabeledInstruction`s
/// pairs of an `Instruction` and an `Option<usize>`.
/// The second element is a unique jump label.
/// Jumps point to these labels instead of the final offset.
///
/// If a instruction is labeled with `i`
/// `Jump(i)` should jump __behind__ it.
///
/// Optimizations run on a vector of `LabeledInstruction`s,
/// then all jumps are rewritten to use relative offsets
/// and the `LabeledInstruction` are converted to normal `Instruction`s.
pub fn convert_instructions(linsts: Vec<LabeledInstruction>) -> Vec<u8> {
    let mut res = vec![];

    // Mapping from label id to instruction index
    let mut labels: HashMap<u32, u32> = HashMap::new();
    let mut pos = 0;

    for &(ref inst, ref label) in linsts.iter() {
        pos += inst.size();
        if let Some(idx) = *label {
            labels.insert(idx as u32, pos as u32);
        }
    }

    // For each jump, look up the index of its label
    // and rewrite it as a relative jump.
    pos = 0;
    for (inst, _label) in linsts.into_iter() {
        pos += inst.size();
        let i = pos as u32;
        match inst {
            Instruction::Jump(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::Jump(to - i).encode());
            }
            Instruction::JumpFalse(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::JumpFalse(to - i).encode());
            }
            Instruction::JumpTrue(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::JumpTrue(to - i).encode());
            }
            Instruction::JumpNil(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::JumpNil(to - i).encode());
            }
            Instruction::JumpNotNil(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::JumpNotNil(to - i).encode());
            }
            Instruction::JumpZero(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::JumpZero(to - i).encode());
            }
            Instruction::JumpNotZero(to_label) => {
                let to = labels[&to_label];
                res.extend(Instruction::JumpNotZero(to - i).encode());
            }
            other => res.extend(other.encode()),
        }
    }

    res
}
