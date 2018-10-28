use std::collections::HashMap;

use byteorder::{LittleEndian, WriteBytesExt};

#[derive(Clone, Copy)]
#[repr(usize)]
pub enum Instruction {
    Inc, Dec, Add, Sub, Mul, Div, Mod, IntDiv,
    Fst, Rst, Cons, Not,
    Equal, Eq, Neq, Gt, Gte, Lt, Lte,
    IsZero, IsNil,
    VectorRef, VectorSet,
    PushValue, PopFunction,
    FunctionInvoke(bool),
    PreserveEnv, RestoreEnv, ExtendEnv, UnlinkEnv,
    Constant(u16),
    PushConstant(u16),
    // Calls to builtin functions
    // are made through references
    // into the lists of builtin functions
    Call1(u16), Call2(u16), Call3(u16), CallN(u16, u8),
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
    Jump(u32),
    JumpFalse(u32),
    JumpTrue(u32),
    JumpNil(u32),
    JumpNotNil(u32),
    JumpZero(u32),
    JumpNotZero(u32),
    FixClosure(u16),
    DottedClosure(u16),
    StoreArgument(u8),
    ConsArgument(u8),
    AllocateFrame(u8),
    AllocateFillFrame(u8),
    AllocateDottedFrame(u8),
    Return,
    Finish,
}

impl Instruction {
    pub fn encode(&self) -> Vec<u8> {
        match self {
            Instruction::Return => vec![0x00_u8],
            Instruction::Finish => vec![0x01_u8],

            Instruction::Inc => vec![0x10_u8],
            Instruction::Dec => vec![0x11_u8],
            Instruction::Add => vec![0x12_u8],
            Instruction::Sub => vec![0x13_u8],
            Instruction::Mul => vec![0x14_u8],
            Instruction::Div => vec![0x15_u8],
            Instruction::Mod => vec![0x16_u8],
            Instruction::IntDiv => vec![0x17_u8],

            Instruction::Not => vec![0x18_u8],
            Instruction::Equal => vec![0x19_u8],
            Instruction::Eq => vec![0x1A_u8],
            Instruction::Neq => vec![0x1B_u8],
            Instruction::Gt => vec![0x1C_u8],
            Instruction::Gte => vec![0x1D_u8],
            Instruction::Lt => vec![0x1E_u8],
            Instruction::Lte => vec![0x1F_u8],

            Instruction::Fst => vec![0x20_u8],
            Instruction::Rst => vec![0x21_u8],
            Instruction::Cons => vec![0x22_u8],
            Instruction::IsZero => vec![0x23_u8],
            Instruction::IsNil => vec![0x24_u8],
            Instruction::VectorRef => vec![0x25_u8],
            Instruction::VectorSet => vec![0x26_u8],

            Instruction::Constant(index) => {
                let mut res = vec![0x30_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::PushConstant(index) => {
                let mut res = vec![0x31_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::PushValue => vec![0x32_u8],
            Instruction::PopFunction => vec![0x33_u8],
            Instruction::PreserveEnv => vec![0x34_u8],
            Instruction::RestoreEnv => vec![0x35_u8],
            Instruction::ExtendEnv => vec![0x36_u8],
            Instruction::UnlinkEnv => vec![0x37_u8],

            Instruction::CheckedGlobalRef(index) => {
                let mut res = vec![0x40_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::GlobalRef(index) => {
                let mut res = vec![0x41_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::PushCheckedGlobalRef(index) => {
                let mut res = vec![0x42_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::PushGlobalRef(index) => {
                let mut res = vec![0x43_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::GlobalSet(index) => {
                let mut res = vec![0x44_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }

            Instruction::Call1(index) => {
                let mut res = vec![0x50_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::Call2(index) => {
                let mut res = vec![0x51_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::Call3(index) => {
                let mut res = vec![0x52_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::CallN(index, argc) => {
                let mut res = vec![0x53_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res.write_u8(*argc).unwrap();
                res
            }

            Instruction::ShallowArgumentRef(index) => {
                let mut res = vec![0x60_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::PushShallowArgumentRef(index) => {
                let mut res = vec![0x61_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::ShallowArgumentSet(index) => {
                let mut res = vec![0x62_u8];
                res.write_u16::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::DeepArgumentRef(index1, index2) => {
                let mut res = vec![0x63_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            }
            Instruction::PushDeepArgumentRef(index1, index2) => {
                let mut res = vec![0x64_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            }
            Instruction::DeepArgumentSet(index1, index2) => {
                let mut res = vec![0x65_u8];
                res.write_u16::<LittleEndian>(*index1).unwrap();
                res.write_u16::<LittleEndian>(*index2).unwrap();
                res
            }

            Instruction::Jump(index) => {
                let mut res = vec![0x70_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::JumpTrue(index) => {
                let mut res = vec![0x71_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::JumpFalse(index) => {
                let mut res = vec![0x72_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::JumpNil(index) => {
                let mut res = vec![0x73_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::JumpNotNil(index) => {
                let mut res = vec![0x74_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::JumpZero(index) => {
                let mut res = vec![0x75_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }
            Instruction::JumpNotZero(index) => {
                let mut res = vec![0x76_u8];
                res.write_u32::<LittleEndian>(*index).unwrap();
                res
            }

            Instruction::FixClosure(arity) => {
                let mut res = vec![0x80_u8];
                res.write_u16::<LittleEndian>(*arity).unwrap();
                res
            }
            Instruction::DottedClosure(arity) => {
                let mut res = vec![0x81_u8];
                res.write_u16::<LittleEndian>(*arity).unwrap();
                res
            }
            Instruction::StoreArgument(index) => {
                let mut res = vec![0x82_u8];
                res.write_u8(*index).unwrap();
                res
            }
            Instruction::ConsArgument(index) => {
                let mut res = vec![0x83_u8];
                res.write_u8(*index).unwrap();
                res
            }
            Instruction::AllocateFrame(index) => {
                let mut res = vec![0x84_u8];
                res.write_u8(*index).unwrap();
                res
            }
            Instruction::AllocateFillFrame(index) => {
                let mut res = vec![0x85_u8];
                res.write_u8(*index).unwrap();
                res
            }
            Instruction::AllocateDottedFrame(index) => {
                let mut res = vec![0x86_u8];
                res.write_u8(*index).unwrap();
                res
            }
            Instruction::FunctionInvoke(tail) => if *tail { vec![0x88] } else { vec![0x87] },
        }
    }

    /// Size of each instruction in bytes
    pub fn size(&self) -> usize {
        use self::Instruction::*;

        match self {
            Return | Finish | Inc | Dec | Add | Sub | Mul | Div | Mod | IntDiv | Not | Equal |
            Eq | Neq | Gt | Gte | Lt | Lte | Fst | Rst | Cons | IsZero | IsNil | VectorRef |
            VectorSet => 1,
            Constant(_) | PushConstant(_) => 3,
            PushValue | PopFunction | PreserveEnv | RestoreEnv | ExtendEnv | UnlinkEnv => 1,

            CheckedGlobalRef(_) => 3,
            GlobalRef(_) => 3,
            PushCheckedGlobalRef(_) => 3,
            PushGlobalRef(_) => 3,
            GlobalSet(_) => 3,
            Call1(_) | Call2(_) | Call3(_) => 3,
            CallN(_, _) => 4,

            ShallowArgumentRef(_) => 3,
            PushShallowArgumentRef(_) => 3,
            ShallowArgumentSet(_) => 3,
            DeepArgumentRef(_, _) => 5,
            PushDeepArgumentRef(_, _) => 5,
            DeepArgumentSet(_, _) => 5,

            Jump(_) | JumpTrue(_) | JumpFalse(_) | JumpNil(_) | JumpNotNil(_) | JumpZero(_) |
            JumpNotZero(_) => 5,

            FixClosure(_) => 3,
            DottedClosure(_) => 3,
            StoreArgument(_) => 2,
            ConsArgument(_) => 2,
            AllocateFrame(_) => 2,
            AllocateFillFrame(_) => 2,
            AllocateDottedFrame(_) => 2,
            FunctionInvoke(_) => 1,
        }
    }
}

pub type LabeledInstruction = (Instruction, Option<usize>);

/// Convert a list of LabeledInstructions to a list of bytes
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
        if let &Some(idx) = label {
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
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::Jump(to - i).encode());
            }
            Instruction::JumpFalse(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpFalse(to - i).encode());
            }
            Instruction::JumpTrue(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpTrue(to - i).encode());
            }
            Instruction::JumpNil(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpNil(to - i).encode());
            }
            Instruction::JumpNotNil(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpNotNil(to - i).encode());
            }
            Instruction::JumpZero(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpZero(to - i).encode());
            }
            Instruction::JumpNotZero(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpNotZero(to - i).encode());
            }
            other => res.extend(other.encode()),
        }

    }

    return res;
}
