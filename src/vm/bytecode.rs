/// A buffer for bytecode, with helper functions for reading numbers
/// of different length
pub struct Bytecode {
    data: Vec<u8>,
    pub pc: usize,
}

impl Bytecode {
    pub fn new(data: Vec<u8>, pc: usize) -> Self {
        Self { data, pc }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn extend(&mut self, instructions: Vec<u8>) {
        self.data.extend(instructions)
    }

    pub fn fetch_u32(&mut self) -> u32 {
        let mut res = u32::from(self.data[self.pc]);
        res += u32::from(self.data[self.pc + 1]) << 8;
        res += u32::from(self.data[self.pc + 2]) << 8;
        res += u32::from(self.data[self.pc + 3]) << 8;
        self.pc += 4;
        res
    }

    fn fetch_u32_as_usize(&mut self) -> usize {
        let mut res = usize::from(self.data[self.pc]);
        res += usize::from(self.data[self.pc + 1]) << 8;
        res += usize::from(self.data[self.pc + 2]) << 8;
        res += usize::from(self.data[self.pc + 3]) << 8;
        self.pc += 4;
        res
    }

    fn fetch_u16(&mut self) -> u16 {
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
