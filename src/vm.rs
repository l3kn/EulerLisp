use Datum;
use IntegerDiv;
use LispFnType;

use builtin::BuiltinRegistry;

use env::{Env, EnvRef};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::io::Write;
use std::rc::Rc;
use symbol_table::SymbolTable;

pub enum VMError {
    EnvStackUnderflow(usize),
    PCStackUnderflow(usize),
    StackUnderflow(usize),
    InstructionFetchError,
}

type VMResult = Result<(), VMError>;

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VMError::EnvStackUnderflow(inst) => write!(f, "Environment Stack Underflow @ {}", inst),
            VMError::PCStackUnderflow(inst) => write!(f, "PC Stack Underflow @ {}", inst),
            VMError::StackUnderflow(inst) => write!(f, "Stack Underflow @ {}", inst),
            VMError::InstructionFetchError => write!(f, "Instruction fetch error"),
        }
    }
}

pub struct VM {
    pub val: Datum,
    fun: Datum,
    env: EnvRef,
    stack: Vec<Datum>,
    env_stack: Vec<EnvRef>,
    pc_stack: Vec<usize>,
    global_env: Vec<Datum>,
    pub bytecode: Vec<u8>,
    pub output: Rc<RefCell<Write>>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    pc: usize,
    constants: Vec<Datum>,
    builtins: BuiltinRegistry,
}

impl VM {
    pub fn new(
        output: Rc<RefCell<Write>>,
        symbol_table: Rc<RefCell<SymbolTable>>,
        builtins: BuiltinRegistry,
    ) -> VM {
        let stack = Vec::with_capacity(1000);
        let local_env = Env::new(None);

        // Start with one "Finish" instruction,
        // the pc pointing behind it and an empty pc stack.
        // This way the last return (e.g. from a tail optimized function)
        // ends the execution.
        VM {
            bytecode: vec![0x01_u8],
            symbol_table,
            builtins,
            output,
            global_env: Vec::new(),
            val: Datum::Undefined,
            fun: Datum::Undefined,
            env: Rc::new(RefCell::new(local_env)),
            stack,
            env_stack: Vec::new(),
            pc_stack: vec![0],
            pc: 1,
            constants: Vec::new(),
        }
    }

    pub fn set_pc(&mut self, v: usize) {
        self.pc = v;
    }

    pub fn add_global(&mut self, g: Datum) {
        self.global_env.push(g);
    }

    pub fn append_instructions(&mut self, insts: Vec<u8>) {
        self.bytecode.extend(insts);
    }

    pub fn append_constants(&mut self, consts: Vec<Datum>) {
        self.constants.extend(consts);
    }

    pub fn reserve_global_vars(&mut self, count: usize) {
        self.global_env.reserve(count);
        for _ in 0..count {
            self.global_env.push(Datum::Undefined);
        }
    }

    fn checked_pop(&mut self) -> Result<Datum, VMError> {
        if let Some(dat) = self.stack.pop() {
            Ok(dat)
        } else {
            Err(VMError::StackUnderflow(self.pc))
        }
    }

    fn fetch_u32(&mut self) -> u32 {
        let mut res = self.bytecode[self.pc + 0] as u32;
        res += (self.bytecode[self.pc + 1] as u32) << 8;
        res += (self.bytecode[self.pc + 2] as u32) << 8;
        res += (self.bytecode[self.pc + 3] as u32) << 8;
        self.pc += 4;
        res
    }

    fn fetch_u32_usize(&mut self) -> usize {
        let mut res = self.bytecode[self.pc + 0] as usize;
        res += (self.bytecode[self.pc + 1] as usize) << 8;
        res += (self.bytecode[self.pc + 2] as usize) << 8;
        res += (self.bytecode[self.pc + 3] as usize) << 8;
        self.pc += 4;
        res
    }

    fn fetch_u16(&mut self) -> u16 {
        let mut res = self.bytecode[self.pc + 0] as u16;
        res += (self.bytecode[self.pc + 1] as u16) << 8;
        self.pc += 2;
        res
    }

    fn fetch_u16_usize(&mut self) -> usize {
        let mut res = self.bytecode[self.pc + 0] as usize;
        res += (self.bytecode[self.pc + 1] as usize) << 8;
        self.pc += 2;
        res
    }

    fn fetch_u8(&mut self) -> u8 {
        let res = self.bytecode[self.pc + 0];
        self.pc += 1;
        res
    }

    fn fetch_u8_usize(&mut self) -> usize {
        let res = self.bytecode[self.pc + 0];
        self.pc += 1;
        res as usize
    }

    fn seek_current(&mut self, offset: u32) {
        self.pc += offset as usize;
    }

    fn seek_start(&mut self, offset: usize) {
        self.pc = offset as usize;
    }

    pub fn run(&mut self) -> VMResult {
        let end = self.bytecode.len();

        loop {
            // TODO: Propagate errors
            let inst = self.fetch_u8();
            match inst {
                // Return
                0x00_u8 => {
                    if let Some(pc) = self.pc_stack.pop() {
                        self.seek_start(pc);
                    } else {
                        return Err(VMError::PCStackUnderflow(self.pc));
                    }
                }
                // Finish
                0x01_u8 => break,
                // Inc
                0x10_u8 => {
                    self.val = match self.val.take() {
                        Datum::Integer(x) => Datum::Integer(x + 1),
                        Datum::Float(x) => Datum::Float(x + 1.0),
                        Datum::Rational(x) => Datum::Rational(x + 1),
                        other => panic!("INC not implemented for {:?}", other),
                    }
                }
                // Dec
                0x11_u8 => {
                    self.val = match self.val.take() {
                        Datum::Integer(x) => Datum::Integer(x - 1),
                        Datum::Float(x) => Datum::Float(x - 1.0),
                        Datum::Rational(x) => Datum::Rational(x - 1),
                        other => panic!("DEC not implemented for {:?}", other),
                    }
                }
                // Add
                0x12_u8 => self.val = self.checked_pop()? + self.val.take(),
                // Sub
                0x13_u8 => self.val = self.checked_pop()? - self.val.take(),
                // Mul
                0x14_u8 => self.val = self.checked_pop()? * self.val.take(),
                // Div
                0x15_u8 => self.val = self.checked_pop()? / self.val.take(),
                // Mod
                0x16_u8 => self.val = self.checked_pop()? % self.val.take(),
                // IntDiv
                0x17_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a.int_div(b);
                }

                // Not
                0x18_u8 => {
                    self.val = Datum::Bool(self.val == Datum::Bool(false));
                }
                // Equal
                0x19_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.is_equal(&self.val).unwrap());
                }
                // Eq
                0x1A_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a == self.val);
                }
                // Neq
                0x1B_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a != self.val);
                }
                // Gt
                0x1C_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() == Ordering::Greater);
                }
                // Gte
                0x1D_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() != Ordering::Less);
                }
                // Lt
                0x1E_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() == Ordering::Less);
                }
                // Lte
                0x1F_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() != Ordering::Greater);
                }

                // Fst
                0x20_u8 => {
                    let a = self.val.take();
                    self.val = a.as_pair().unwrap().0.clone();
                }
                // Rst
                0x21_u8 => {
                    let a = self.val.take();
                    self.val = a.as_pair().unwrap().1.clone();
                }
                // Cons
                0x22_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = Datum::make_pair(a, b);
                }
                // IsZero
                0x23_u8 => {
                    self.val = Datum::Bool(self.val.is_equal(&Datum::Integer(0)).unwrap());
                }
                // IsNil
                0x24_u8 => self.val = Datum::Bool(self.val == Datum::Nil),
                // VectorRef
                0x25_u8 => {
                    let vector = self.checked_pop()?;
                    let index = self.val.take();

                    // TODO: Convert errors
                    let vector = vector.as_vector().unwrap();
                    match vector.get(index.as_uinteger().unwrap()) {
                        Some(e) => self.val = e.clone(),
                        None => panic!("vector-ref index out of bounds"),
                    }
                }
                // VectorSet
                0x26_u8 => {
                    let index = self.checked_pop()?;
                    let vector = self.checked_pop()?;
                    let value = self.val.take();

                    // TODO: Convert errors
                    let mut vector = vector.as_mut_vector().unwrap();
                    let index = index.as_uinteger().unwrap();
                    if index < vector.len() {
                        vector[index] = value;
                    } else {
                        panic!("vector-set index out of bounds")
                    }
                }

                // Constant
                0x30_u8 => {
                    let i = self.fetch_u16_usize();
                    self.val = self.constants[i].clone();
                }
                // PushConstant
                0x31_u8 => {
                    let i = self.fetch_u16_usize();
                    self.stack.push(self.constants[i].clone());
                }
                // PushValue
                0x32_u8 => self.stack.push(self.val.take()),
                // PopFunction
                0x33_u8 => self.fun = self.checked_pop()?,
                // PreserveEnv
                0x34_u8 => {
                    self.env_stack.push(self.env.clone());
                }
                // RestoreEnv
                0x35_u8 => {
                    if let Some(env) = self.env_stack.pop() {
                        self.env = env;
                    } else {
                        return Err(VMError::EnvStackUnderflow(self.pc));
                    }
                }
                // ExtendEnv
                0x36_u8 => {
                    let mut new_env = Env::new(Some(self.env.clone()));
                    if let Datum::ActivationFrame(elems) = self.val.take() {
                        new_env.extend(elems);
                    } else {
                        panic!("ExtendEnv without a activation frame in val");
                    }

                    self.env = Rc::new(RefCell::new(new_env));
                }
                // UnlinkEnv
                0x37_u8 => {
                    let parent = self.env.borrow().parent.clone().unwrap();
                    self.env = parent;
                }

                // CheckedGlobalRef
                0x40_u8 => {
                    let idx = self.fetch_u16_usize();
                    let v = &self.global_env[idx];
                    if *v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.val = v.clone();
                    }
                }
                // GlobalRef
                0x41_u8 => {
                    let idx = self.fetch_u16_usize();
                    let v = &self.global_env[idx];
                    self.val = v.clone();
                }
                // PushCheckedGlobalRef
                0x42_u8 => {
                    let idx = self.fetch_u16_usize();
                    let v = &self.global_env[idx];
                    if *v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.stack.push(v.clone());
                    }
                }
                // CheckedGlobalRef
                0x43_u8 => {
                    let idx = self.fetch_u16_usize();
                    let v = &self.global_env[idx];
                    self.stack.push(v.clone());
                }
                // GlobalSet
                0x44_u8 => {
                    let idx = self.fetch_u16_usize();
                    self.global_env[idx] = self.val.take();
                }

                // Call1
                0x50_u8 => {
                    let idx = self.fetch_u16_usize();
                    let res = self.builtins.call_1(idx, self.val.take(), &self);
                    self.val = res.unwrap();
                }
                // Call2
                0x51_u8 => {
                    let idx = self.fetch_u16_usize();
                    let arg1 = self.checked_pop()?;
                    let res = self.builtins.call_2(idx, arg1, self.val.take(), &self);
                    self.val = res.unwrap();
                }
                // Call3
                0x52_u8 => {
                    let idx = self.fetch_u16_usize();
                    let arg2 = self.checked_pop()?;
                    let arg1 = self.checked_pop()?;
                    let res = self
                        .builtins
                        .call_3(idx, arg1, arg2, self.val.take(), &self);
                    self.val = res.unwrap();
                }
                // CallN
                0x53_u8 => {
                    let idx = self.fetch_u16_usize();
                    let given = self.fetch_u8() as usize;
                    let at = self.stack.len() - given;
                    let mut args = self.stack.split_off(at);
                    let res = self.builtins.call_n(idx, &mut args, &self);
                    self.val = res.unwrap()
                }

                // ShallowArgumentRef
                0x60_u8 => {
                    let j = self.fetch_u16_usize();
                    let env = self.env.borrow();
                    self.val = env.shallow_ref(j);
                }
                // PushShallowArgumentRef
                0x61_u8 => {
                    let j = self.fetch_u16_usize();
                    let env = self.env.borrow();
                    self.stack.push(env.shallow_ref(j));
                }
                // ShallowArgumentSet
                0x62_u8 => {
                    let j = self.fetch_u16_usize();
                    let mut env = self.env.borrow_mut();
                    env.shallow_set(j, self.val.take());
                }
                // DeepArgumentRef
                0x63_u8 => {
                    let i = self.fetch_u16_usize();
                    let j = self.fetch_u16_usize();
                    let env = self.env.borrow();
                    self.val = env.deep_ref(i, j);
                }
                // PushDeepArgumentRef
                0x64_u8 => {
                    let i = self.fetch_u16_usize();
                    let j = self.fetch_u16_usize();
                    let env = self.env.borrow();
                    self.stack.push(env.deep_ref(i, j));
                }
                // DeepArgumentSet
                0x65_u8 => {
                    let i = self.fetch_u16_usize();
                    let j = self.fetch_u16_usize();
                    let mut env = self.env.borrow_mut();
                    env.deep_set(i, j, self.val.take());
                }

                // Jump
                0x70_u8 => {
                    let offset = self.fetch_u32();
                    self.seek_current(offset);
                }
                // JumpTrue
                0x71_u8 => {
                    let offset = self.fetch_u32();
                    if self.val.is_true() {
                        self.seek_current(offset);
                    }
                }
                // JumpFalse
                0x72_u8 => {
                    let offset = self.fetch_u32();
                    if self.val.is_false() {
                        self.seek_current(offset);
                    }
                }
                // JumpNil
                0x73_u8 => {
                    let offset = self.fetch_u32();
                    if self.val == Datum::Nil {
                        self.seek_current(offset);
                    }
                }
                // JumpNotNil
                0x74_u8 => {
                    let offset = self.fetch_u32();
                    if self.val != Datum::Nil {
                        self.seek_current(offset);
                    }
                }
                // JumpZero
                0x75_u8 => {
                    let offset = self.fetch_u32();
                    if self.val.is_equal(&Datum::Integer(0)).unwrap() {
                        self.seek_current(offset);
                    }
                }
                // JumpNotZero
                0x76_u8 => {
                    let offset = self.fetch_u32();
                    if !self.val.is_equal(&Datum::Integer(0)).unwrap() {
                        self.seek_current(offset);
                    }
                }

                // FixClosure
                0x80_u8 => {
                    let arity = self.fetch_u16_usize();

                    // The next instruction, a jump behind the closure,
                    // needs to be scipped (1 byte type + 4 bytes addr)
                    let closure = Datum::Closure(self.pc + 5, arity, false, self.env.clone());
                    self.val = closure;
                }
                // DottedClosure
                0x81_u8 => {
                    let arity = self.fetch_u16_usize();

                    let closure = Datum::Closure(self.pc + 5, arity, true, self.env.clone());
                    self.val = closure;
                }
                // StoreArgument
                0x82_u8 => {
                    unimplemented!();
                    // let idx = self.fetch_u8_usize();
                    // self.frame[idx] = self.checked_pop()?;
                }
                // ConsArgument
                0x83_u8 => {
                    unimplemented!();
                    // let idx = self.fetch_u8_usize();
                    // self.frame[idx] = Datum::make_pair(self.frame[idx].take(), self.val.take());
                }
                // AllocateFrame
                0x84_u8 => {
                    unimplemented!();
                    // let size = self.fetch_u8_usize();
                    // self.frame = Vec::with_capacity(size);
                    // for _ in 0..size {
                    //     self.frame.push(Datum::Undefined);
                    // }
                }
                // AllocateFillFrame
                0x85_u8 => {
                    let size = self.fetch_u8_usize();
                    let mut frame = Vec::with_capacity(size);
                    // self.frame = Vec::with_capacity(size);
                    for _ in 0..size {
                        let v = self.checked_pop()?;
                        frame.push(v);
                        // self.frame.push(v);
                    }
                    self.val = Datum::ActivationFrame(frame);
                }
                // AllocateDottedFrame
                //
                // The same as `AllocateFrame`,
                // just sets the last element to '()
                // so that `ConsArgument` can add the dotted arguments to it
                0x86_u8 => {
                    unimplemented!();
                    // let size = self.fetch_u8_usize();
                    // self.frame = Vec::with_capacity(size);
                    // for _ in 0..(size - 1) {
                    //     self.frame.push(Datum::Undefined);
                    // }
                    // self.frame.push(Datum::Nil);
                }
                // FunctionInvoke
                v @ 0x87_u8 | v @ 0x88_u8 => {
                    let is_tail = v == 0x88_u8;

                    // TODO: Include function symbol for debugging
                    if let Datum::Closure(offset, arity, dotted, ref env) = self.fun {
                        if !is_tail {
                            self.pc_stack.push(self.pc);
                        }

                        if let Datum::ActivationFrame(mut elems) = self.val.take() {
                            if dotted {
                                if (elems.len() + 1) < arity {
                                    panic!("Incorrect arity");
                                }

                                let rest = elems.split_off(arity - 1);
                                elems.push(Datum::make_list_from_vec(rest));

                                let mut new_env = Env::new(Some(env.clone()));
                                new_env.extend(elems.clone());

                                self.env = Rc::new(RefCell::new(new_env));
                                self.pc = offset;
                            } else {
                                let got = elems.len();
                                if arity != elems.len() {
                                    panic!("Incorrect arity, expected {}, got {}", arity, got);
                                }
                                let mut new_env = Env::new(Some(env.clone()));
                                new_env.extend(elems);

                                self.env = Rc::new(RefCell::new(new_env));
                                self.pc = offset;
                            }
                        } else {
                            panic!("Clojure invocation without activation frame in val");
                        }
                    } else if let Datum::Builtin(ref typ, idx, ref arity) = self.fun {
                        let idx = idx as usize;

                        if let Datum::ActivationFrame(mut elems) = self.val.take() {
                            arity.check(elems.len());
                            match typ {
                                LispFnType::Variadic => {
                                    // let mut args: Vec<Datum> =
                                    //     elems.iter_mut().map(|d| d.take()).collect();
                                    // let res = self.builtins.call_n(idx, &mut args, &self);
                                    let res = self.builtins.call_n(idx, &mut elems, &self);
                                    self.val = res.unwrap();
                                }
                                LispFnType::Fixed1 => {
                                    let arg1 = elems[0].take();
                                    let res = self.builtins.call_1(idx, arg1, &self);
                                    self.val = res.unwrap();
                                }
                                LispFnType::Fixed2 => {
                                    let arg1 = elems[0].take();
                                    let arg2 = elems[1].take();
                                    let res = self.builtins.call_2(idx, arg1, arg2, &self);
                                    self.val = res.unwrap();
                                }
                                LispFnType::Fixed3 => {
                                    let arg1 = elems[0].take();
                                    let arg2 = elems[1].take();
                                    let arg3 = elems[2].take();
                                    let res = self.builtins.call_3(idx, arg1, arg2, arg3, &self);
                                    self.val = res.unwrap();
                                }
                            }
                        } else {
                            panic!("Builtin invocation without activation frame in val");
                        }
                    } else {
                        panic!("Trying to invoke non function {:?}", self.fun);
                    }
                }
                _ => unimplemented!(),
            }

            if self.pc == end {
                break;
            }
        }

        Ok(())
    }
}
