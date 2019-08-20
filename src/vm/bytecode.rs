use super::error::Error;
use crate::instruction::{convert_instructions, LabeledInstruction};

/// A buffer for bytecode, with helper functions for reading numbers
/// of different length
pub struct Bytecode {
    data: Vec<u8>,
    pub pc: usize,
    pc_stack: Vec<usize>,
}

impl Bytecode {
    pub fn new(data: Vec<u8>, pc: usize) -> Self {
        Self {
            data,
            pc,
            // TODO: This should not be necessary
            pc_stack: vec![0],
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn extend(&mut self, instructions: Vec<LabeledInstruction>) {
        self.data.extend(convert_instructions(instructions))
    }

    pub fn set_pc(&mut self, v: usize) {
        self.pc = v;
    }

    pub fn inc_pc(&mut self, v: usize) {
        self.pc += v;
    }

    pub fn store_pc(&mut self) {
        self.pc_stack.push(self.pc);
    }

    pub fn restore_pc(&mut self) -> Result<(), Error> {
        if let Some(pc) = self.pc_stack.pop() {
            self.pc = pc;
            Ok(())
        } else {
            Err(Error::PCStackUnderflow(self.pc))
        }
    }

    pub fn fetch_u32(&mut self) -> u32 {
        let mut res = u32::from(self.data[self.pc]);
        res += u32::from(self.data[self.pc + 1]) << 8;
        res += u32::from(self.data[self.pc + 2]) << 8;
        res += u32::from(self.data[self.pc + 3]) << 8;
        self.pc += 4;
        res
    }

    pub fn fetch_u32_as_usize(&mut self) -> usize {
        let mut res = usize::from(self.data[self.pc]);
        res += usize::from(self.data[self.pc + 1]) << 8;
        res += usize::from(self.data[self.pc + 2]) << 8;
        res += usize::from(self.data[self.pc + 3]) << 8;
        self.pc += 4;
        res
    }

    pub fn fetch_u16(&mut self) -> u16 {
        let mut res = u16::from(self.data[self.pc]);
        res += u16::from(self.data[self.pc + 1]) << 8;
        self.pc += 2;
        res
    }

    pub fn fetch_u16_as_usize(&mut self) -> usize {
        let mut res = usize::from(self.data[self.pc]);
        res += usize::from(self.data[self.pc + 1]) << 8;
        self.pc += 2;
        res
    }

    pub fn fetch_u8(&mut self) -> u8 {
        let res = self.data[self.pc];
        self.pc += 1;
        res
    }

    pub fn fetch_u8_as_usize(&mut self) -> usize {
        let res = self.data[self.pc];
        self.pc += 1;
        res as usize
    }
}
