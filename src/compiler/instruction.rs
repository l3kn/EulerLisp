use std::fmt;

use byteorder::{LittleEndian, WriteBytesExt};

#[derive(Clone, Copy)]
#[repr(usize)]
pub enum Instruction {
    Inc, Dec,
    Add, Sub, Mul, Div, Mod, IntDiv,
    Fst, Rst, Cons,
    Not,
    Equal, Eq, Neq,
    Gt, Gte, Lt, Lte,
    IsZero, IsNil,
    VectorRef,
    VectorSet,
    PushValue,
    PopFunction,
    FunctionInvoke(bool),
    PreserveEnv, RestoreEnv, ExtendEnv, UnlinkEnv,
    Constant(u16),
    PushConstant(u16),
    // Calls to builtin functions
    // are made through references
    // into the lists of builtin functions
    Call1(u16),
    Call2(u16),
    Call3(u16),
    CallN(u16, u8),
    CheckedGlobalRef(u32),
    GlobalRef(u32),
    PushCheckedGlobalRef(u32),
    PushGlobalRef(u32),
    GlobalSet(u32),
    ShallowArgumentRef(u16),
    PushShallowArgumentRef(u16),
    ShallowArgumentSet(u16),
    DeepArgumentRef(u16, u16),
    PushDeepArgumentRef(u16, u16),
    DeepArgumentSet(u16, u16),
    Jump(u32),
    JumpFalse(u32),
    JumpTrue(u32),
    JumpNil(u32),
    JumpNotNil(u32),
    JumpZero(u32),
    JumpNotZero(u32),
    // TODO: It seems like the first argument is always 1
    FixClosure(u16, u16),
    DottedClosure(u16, u16),
    StoreArgument(u32),
    ConsArgument(u32),
    AllocateFrame(u32),
    AllocateFillFrame(u32),
    AllocateDottedFrame(u32),
    Return,
    Finish,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Finish => write!(f, "FINISH"),
            Instruction::Inc => write!(f, "INC"),
            Instruction::Dec => write!(f, "DEC"),
            Instruction::Add => write!(f, "ADD"),
            Instruction::Sub => write!(f, "SUB"),
            Instruction::Mul => write!(f, "MUL"),
            Instruction::Div => write!(f, "DIV"),
            Instruction::Mod => write!(f, "MOD"),
            Instruction::IntDiv => write!(f, "IDIV"),
            Instruction::Fst => write!(f, "FST"),
            Instruction::Rst => write!(f, "RST"),
            Instruction::Cons => write!(f, "CONS"),
            Instruction::Not => write!(f, "NOT"),
            Instruction::Eq => write!(f, "EQ"),
            Instruction::Neq => write!(f, "NEQ"),
            Instruction::Equal => write!(f, "EQUAL"),
            Instruction::Lt => write!(f, "LT"),
            Instruction::Gt => write!(f, "GT"),
            Instruction::Lte => write!(f, "LTE"),
            Instruction::Gte => write!(f, "GTE"),
            Instruction::IsZero => write!(f, "ZERO?"),
            Instruction::IsNil => write!(f, "NIL?"),
            Instruction::VectorRef => write!(f, "VECTOR-REF"),
            Instruction::VectorSet => write!(f, "VECTOR-SET!"),
            Instruction::Constant(i) => write!(f, "CONSTANT {}", i),
            Instruction::PushConstant(i) => write!(f, "PUSH-CONSTANT {}", i),
            Instruction::PushValue => write!(f, "PUSH-VALUE"),
            Instruction::GlobalSet(i) => write!(f, "GLOBAL-SET {}", i),
            Instruction::GlobalRef(i) => write!(f, "GLOBAL-REF {}", i),
            Instruction::PushGlobalRef(i) => write!(f, "PUSH-GLOBAL-REF {}", i),
            Instruction::CheckedGlobalRef(i) => write!(f, "CHECKED-GLOBAL-REF {}", i),
            Instruction::PushCheckedGlobalRef(i) => write!(f, "PUSH-CHECKED-GLOBAL-REF {}", i),
            Instruction::ShallowArgumentRef(i) => write!(f, "SHALLOW-ARGUMENT-REF {}", i),
            Instruction::PushShallowArgumentRef(i) => write!(f, "PUSH-SHALLOW-ARGUMENT-REF {}", i),
            Instruction::ShallowArgumentSet(i) => write!(f, "SHALLOW-ARGUMENT-SET {}", i),
            Instruction::DeepArgumentRef(i, j) => write!(f, "DEEP-ARGUMENT-REF {} {}", i, j),
            Instruction::PushDeepArgumentRef(i, j) => write!(f, "PUSH-DEEP-ARGUMENT-REF {} {}", i, j),
            Instruction::DeepArgumentSet(i, j) => write!(f, "DEEP-ARGUMENT-SET {} {}", i, j),
            Instruction::PreserveEnv => write!(f, "PRESERVE-ENV"),
            Instruction::RestoreEnv => write!(f, "RESTORE-ENV"),
            Instruction::ExtendEnv => write!(f, "EXTEND-ENV"),
            Instruction::UnlinkEnv => write!(f, "UNLINK-ENV"),
            // TODO: Find some way to get to the function name
            Instruction::Call1(_) => write!(f, "CALL1"),
            Instruction::Call2(_) => write!(f, "CALL2"),
            Instruction::Call3(_) => write!(f, "CALL3"),
            Instruction::CallN(_, arity) => write!(f, "CALLN {}", arity),
            Instruction::Jump(offset) => write!(f, "JUMP +{}", offset),
            Instruction::JumpFalse(offset) => write!(f, "JUMP-FALSE +{}", offset),
            Instruction::JumpTrue(offset) => write!(f, "JUMP-TRUE +{}", offset),
            Instruction::JumpNil(offset) => write!(f, "JUMP-NIL +{}", offset),
            Instruction::JumpNotNil(offset) => write!(f, "JUMP-NOT-NIL +{}", offset),
            Instruction::JumpZero(offset) => write!(f, "JUMP-ZERO +{}", offset),
            Instruction::JumpNotZero(offset) => write!(f, "JUMP-NOT-ZERO +{}", offset),
            Instruction::FixClosure(offset, arity) => write!(f, "CREATE-CLOSURE +{} {}", offset, arity),
            Instruction::DottedClosure(offset, arity) => write!(f, "CREATE-CLOSURE +{} {}", offset, arity),
            Instruction::Return => write!(f, "RETURN"),
            Instruction::StoreArgument(idx) => write!(f, "STORE-ARGUMENT {}", idx),
            Instruction::ConsArgument(idx) => write!(f, "CONS-ARGUMENT {}", idx),
            Instruction::AllocateFrame(idx) => write!(f, "ALLOCATE-FRAME {}", idx),
            Instruction::AllocateFillFrame(idx) => write!(f, "ALLOCATE-FILL-FRAME {}", idx),
            Instruction::AllocateDottedFrame(idx) => write!(f, "ALLOCATE-DOTTED-FRAME {}", idx),
            Instruction::PopFunction => write!(f, "POP-FUNCTION"),
            Instruction::FunctionInvoke(tail) => write!(f, "FUNCTION-INVOKE tail: {}", tail),
        }
    }
}

impl Instruction {
    pub fn encode(&self) -> Vec<u8> {
        match self {
            Instruction::Return => vec![0x00_u8],
            Instruction::Finish => vec![0x01_u8],

            Instruction::Inc    => vec![0x10_u8],
            Instruction::Dec    => vec![0x11_u8],
            Instruction::Add    => vec![0x12_u8],
            Instruction::Sub    => vec![0x13_u8],
            Instruction::Mul    => vec![0x14_u8],
            Instruction::Div    => vec![0x15_u8],
            Instruction::Mod    => vec![0x16_u8],
            Instruction::IntDiv => vec![0x17_u8],

            Instruction::Not    => vec![0x18_u8],
            Instruction::Equal  => vec![0x19_u8],
            Instruction::Eq     => vec![0x1A_u8],
            Instruction::Neq    => vec![0x1B_u8],
            Instruction::Gt     => vec![0x1C_u8],
            Instruction::Gte    => vec![0x1D_u8],
            Instruction::Lt     => vec![0x1E_u8],
            Instruction::Lte    => vec![0x1F_u8],

            Instruction::Fst       => vec![0x20_u8],
            Instruction::Rst       => vec![0x21_u8],
            Instruction::Cons      => vec![0x22_u8],
            Instruction::IsZero    => vec![0x23_u8],
            Instruction::IsNil     => vec![0x24_u8],
            Instruction::VectorRef => vec![0x25_u8],
            Instruction::VectorSet => vec![0x26_u8],

            Instruction::Constant(index) => {
                let mut res = vec![0x30_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::PushConstant(index) => {
                let mut res = vec![0x31_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::PushValue => vec![0x32_u8],
            Instruction::PopFunction => vec![0x33_u8],
            Instruction::PreserveEnv => vec![0x34_u8],
            Instruction::RestoreEnv  => vec![0x35_u8],
            Instruction::ExtendEnv   => vec![0x36_u8],
            Instruction::UnlinkEnv   => vec![0x37_u8],

            Instruction::CheckedGlobalRef(index) => {
                let mut res = vec![0x40_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::GlobalRef(index) => {
                let mut res = vec![0x41_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::PushCheckedGlobalRef(index) => {
                let mut res = vec![0x42_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::PushGlobalRef(index) => {
                let mut res = vec![0x43_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::GlobalSet(index) => {
                let mut res = vec![0x44_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },

            Instruction::Call1(index) => {
                let mut res = vec![0x50_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::Call2(index) => {
                let mut res = vec![0x51_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::Call3(index) => {
                let mut res = vec![0x52_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::CallN(index, argc) => {
                let mut res = vec![0x53_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res.write_u8(*argc).unwrap();
                res
            },

            Instruction::ShallowArgumentRef(index) => {
                let mut res = vec![0x60_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::PushShallowArgumentRef(index) => {
                let mut res = vec![0x61_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::ShallowArgumentSet(index) => {
                let mut res = vec![0x62_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::DeepArgumentRef(index1, index2) => {
                let mut res = vec![0x63_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            },
            Instruction::PushDeepArgumentRef(index1, index2) => {
                let mut res = vec![0x64_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            },
            Instruction::DeepArgumentSet(index1, index2) => {
                let mut res = vec![0x65_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            },

            Instruction::Jump(index) => {
                let mut res = vec![0x70_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::JumpTrue(index) => {
                let mut res = vec![0x71_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::JumpFalse(index) => {
                let mut res = vec![0x72_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::JumpNil(index) => {
                let mut res = vec![0x73_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::JumpNotNil(index) => {
                let mut res = vec![0x74_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::JumpZero(index) => {
                let mut res = vec![0x75_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::JumpNotZero(index) => {
                let mut res = vec![0x76_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },

            Instruction::FixClosure(index1, index2) => {
                let mut res = vec![0x80_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            },
            Instruction::DottedClosure(index1, index2) => {
                let mut res = vec![0x81_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            },
            Instruction::StoreArgument(index) => {
                let mut res = vec![0x82_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::ConsArgument(index) => {
                let mut res = vec![0x83_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::AllocateFrame(index) => {
                let mut res = vec![0x84_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::AllocateFillFrame(index) => {
                let mut res = vec![0x85_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::AllocateDottedFrame(index) => {
                let mut res = vec![0x86_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            },
            Instruction::FunctionInvoke(tail) => {
                if *tail {
                    vec![0x88]
                } else {
                    vec![0x87]
                }
            },
        }
    }

    /// Size of each instruction in bytes
    pub fn size(&self) -> usize {
        match self {
            Instruction::Return => 1,
            Instruction::Finish => 1,
            Instruction::Inc    => 1,
            Instruction::Dec    => 1,
            Instruction::Add    => 1,
            Instruction::Sub    => 1,
            Instruction::Mul    => 1,
            Instruction::Div    => 1,
            Instruction::Mod    => 1,
            Instruction::IntDiv => 1,

            Instruction::Not    => 1,
            Instruction::Equal  => 1,
            Instruction::Eq     => 1,
            Instruction::Neq    => 1,
            Instruction::Gt     => 1,
            Instruction::Gte    => 1,
            Instruction::Lt     => 1,
            Instruction::Lte    => 1,

            Instruction::Fst       => 1,
            Instruction::Rst       => 1,
            Instruction::Cons      => 1,
            Instruction::IsZero    => 1,
            Instruction::IsNil     => 1,
            Instruction::VectorRef => 1,
            Instruction::VectorSet => 1,

            Instruction::Constant(_) => 3,
            Instruction::PushConstant(_) => 3,
            Instruction::PushValue => 1,
            Instruction::PopFunction => 1,
            Instruction::PreserveEnv => 1,
            Instruction::RestoreEnv  => 1,
            Instruction::ExtendEnv   => 1,
            Instruction::UnlinkEnv   => 1,

            Instruction::CheckedGlobalRef(_) => 5,
            Instruction::GlobalRef(_) => 5,
            Instruction::PushCheckedGlobalRef(_) => 5,
            Instruction::PushGlobalRef(_) => 5,
            Instruction::GlobalSet(_) => 5,
            Instruction::Call1(_) => 3,
            Instruction::Call2(_) => 3,
            Instruction::Call3(_) => 3,
            Instruction::CallN(_, _) => 4,

            Instruction::ShallowArgumentRef(_) => 3,
            Instruction::PushShallowArgumentRef(_) => 3,
            Instruction::ShallowArgumentSet(_) => 3,
            Instruction::DeepArgumentRef(_, _) => 5,
            Instruction::PushDeepArgumentRef(_, _) => 5,
            Instruction::DeepArgumentSet(_, _) => 5,

            Instruction::Jump(_) => 5,
            Instruction::JumpTrue(_) => 5,
            Instruction::JumpFalse(_) => 5,
            Instruction::JumpNil(_) => 5,
            Instruction::JumpNotNil(_) => 5,
            Instruction::JumpZero(_) => 5,
            Instruction::JumpNotZero(_) => 5,

            Instruction::FixClosure(_, _) => 5,
            Instruction::DottedClosure(_, _) => 5,
            Instruction::StoreArgument(_) => 5,
            Instruction::ConsArgument(_) => 5,
            Instruction::AllocateFrame(_) => 5,
            Instruction::AllocateFillFrame(_) => 5,
            Instruction::AllocateDottedFrame(_) => 5,
            Instruction::FunctionInvoke(_) => 1,
        }
    }
}
