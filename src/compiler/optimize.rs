use crate::compiler::Instruction;
use crate::compiler::LabeledInstruction;

pub fn optimize(instructions: Vec<LabeledInstruction>) -> Vec<LabeledInstruction> {
    let mut res = Vec::new();
    let mut iter = instructions.into_iter().peekable();

    while let Some(i) = iter.next() {
        match i {
            (Instruction::Constant(d), None) => {
                if let Some(&(Instruction::PushValue, l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::PushConstant(d), l));
                } else {
                    res.push((Instruction::Constant(d), None));
                }
            }
            (Instruction::GlobalRef(i), None) => {
                if let Some(&(Instruction::PushValue, l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::PushGlobalRef(i), l));
                } else {
                    res.push((Instruction::GlobalRef(i), None));
                }
            }
            (Instruction::CheckedGlobalRef(i), None) => {
                if let Some(&(Instruction::PushValue, l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::PushCheckedGlobalRef(i), l));
                } else {
                    res.push((Instruction::CheckedGlobalRef(i), None));
                }
            }
            (Instruction::ShallowArgumentRef(i), None) => {
                if let Some(&(Instruction::PushValue, l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::PushShallowArgumentRef(i), l));
                } else {
                    res.push((Instruction::ShallowArgumentRef(i), None));
                }
            }
            (Instruction::DeepArgumentRef(i, j), None) => {
                if let Some(&(Instruction::PushValue, l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::PushDeepArgumentRef(i, j), l));
                } else {
                    res.push((Instruction::DeepArgumentRef(i, j), None));
                }
            }
            (Instruction::IsNil, None) => {
                if let Some(&(Instruction::JumpFalse(o), l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::JumpNotNil(o), l));
                } else if let Some(&(Instruction::JumpTrue(o), l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::JumpNil(o), l));
                } else {
                    res.push((Instruction::IsNil, None));
                }
            }
            (Instruction::IsZero, None) => {
                if let Some(&(Instruction::JumpFalse(o), l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::JumpNotZero(o), l));
                } else if let Some(&(Instruction::JumpTrue(o), l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::JumpZero(o), l));
                } else {
                    res.push((Instruction::IsZero, None));
                }
            }
            (Instruction::Not, None) => {
                if let Some(&(Instruction::JumpFalse(o), l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::JumpTrue(o), l));
                } else if let Some(&(Instruction::JumpTrue(o), l)) = iter.peek() {
                    iter.next();
                    res.push((Instruction::JumpFalse(o), l));
                } else {
                    res.push((Instruction::Not, None));
                }
            }
            other => res.push(other),
        }
    }

    return res;
}
