use ::Datum;
use ::LispFnType;
use ::IntegerDiv;

use builtin::BuiltinRegistry;

use env::{Env, EnvRef};
use symbol_table::SymbolTable;
use compiler::Instruction;
use std::cell::RefCell;
use std::fmt;
use std::io::{Write, Cursor};
use std::rc::Rc;
use std::cmp::Ordering;

use byteorder::{LittleEndian, ReadBytesExt};

pub enum VMError {
    EnvStackUnderflow(usize),
    PCStackUnderflow(usize),
    StackUnderflow(usize),
    InstructionFetchError
}

type VMResult = Result<(), VMError>;

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VMError::EnvStackUnderflow(inst) => {
                write!(f, "Environment Stack Underflow @ {}", inst)
            },
            VMError::PCStackUnderflow(inst) => {
                write!(f, "PC Stack Underflow @ {}", inst)
            },
            VMError::StackUnderflow(inst) => {
                write!(f, "Stack Underflow @ {}", inst)
            },
            VMError::InstructionFetchError => {
                write!(f, "Instruction fetch error")
            },
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
    pub program: Vec<Instruction>,
    pub bytecode: Cursor<Vec<u8>>,
    pc: usize,
    frame: Vec<Datum>,
    pub output: Rc<RefCell<Write>>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Datum>,
    builtins: BuiltinRegistry
}

impl VM {
    pub fn new(
        output: Rc<RefCell<Write>>,
        symbol_table: Rc<RefCell<SymbolTable>>,
        builtins: BuiltinRegistry,
   ) -> VM {
        let stack = Vec::with_capacity(1000);
        let local_env = Env::new(None);

        VM {
            // FIXME: Because of the way the pc is incremented,
            // jumping to pc 0 is not possible
            program: vec![Instruction::Finish, Instruction::Finish],
            // FIXME: See above for reasons,
            // not an elegant solution
            bytecode: Cursor::new(vec![0x01_u8, 0x01_u8]),
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
            frame: Vec::new(),
            pc: 2,
            constants: Vec::new(),
        }
    }

    pub fn set_pc(&mut self, v: usize) {
        self.pc = v;
    }

    pub fn add_global(&mut self, g: Datum) {
        self.global_env.push(g);
    }

    pub fn append_instructions(&mut self, insts: Vec<Instruction>) {
        for inst in &insts {
            self.bytecode.get_mut().extend(inst.encode());
        }
        self.program.extend(insts);
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

    fn fetch_u32(&mut self) -> Result<u32, VMError> {
        if let Ok(res) = self.bytecode.read_u32::<LittleEndian>() {
            Ok(res)
        } else {
            Err(VMError::InstructionFetchError)
        }
    }

    fn fetch_u16(&mut self) -> Result<u16, VMError> {
        if let Ok(res) = self.bytecode.read_u16::<LittleEndian>() {
            Ok(res)
        } else {
            Err(VMError::InstructionFetchError)
        }
    }

    fn fetch_u8(&mut self) -> Result<u8, VMError> {
        if let Ok(res) = self.bytecode.read_u8() {
            Ok(res)
        } else {
            Err(VMError::InstructionFetchError)
        }
    }

    // TODO:
    //
    // Currently the pc is not handled correctly,
    // in jump instructions the distance should be measured
    // in bytes and not in instructions.
    //
    // Then the new pc should be set via self.bytecode.set_position()
    // instead of using a register.
    pub fn run_(&mut self) -> VMResult {
        loop {
            // TODO: Propagate errors
            match self.bytecode.read_u8().unwrap() {
                // Return
                0x00_u8 => {
                    if let Some(pc) = self.pc_stack.pop() {
                        self.pc = pc;
                    } else {
                        return Err(VMError::PCStackUnderflow(self.pc));
                    }
                },
                // Finish
                0x01_u8 => break,

                // Inc
                0x10_u8 => {
                    self.val = match self.val.take() {
                        Datum::Integer(x) => Datum::Integer(x + 1),
                        Datum::Float(x) => Datum::Float(x + 1.0),
                        Datum::Rational(x) => Datum::Rational(x + 1),
                        other => panic!("INC not implemented for {:?}", other)
                    }
                },
                // Dec
                0x11_u8 => {
                    self.val = match self.val.take() {
                        Datum::Integer(x) => Datum::Integer(x - 1),
                        Datum::Float(x) => Datum::Float(x - 1.0),
                        Datum::Rational(x) => Datum::Rational(x - 1),
                        other => panic!("DEC not implemented for {:?}", other)
                    }
                },
                // Add
                0x12_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a + b;
                },
                // Sub
                0x13_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a - b;
                },
                // Mul
                0x14_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a * b;
                },
                // Div
                0x15_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a / b;
                },
                // Mod
                0x16_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a % b;
                },
                // IntDiv
                0x17_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a.int_div(b);
                },

                // Not
                0x18_u8 => {
                    self.val = Datum::Bool(self.val == Datum::Bool(false));
                },
                // Equal
                0x19_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.is_equal(&self.val).unwrap());
                },
                // Eq
                0x1A_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a == self.val);
                },
                // Neq
                0x1B_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a != self.val);
                },
                // Gt
                0x1C_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() == Ordering::Greater);
                },
                // Gte
                0x1D_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() != Ordering::Less);
                },
                // Lt
                0x1E_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() == Ordering::Less);
                },
                // Lte
                0x1F_u8 => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() != Ordering::Greater);
                },

                // Fst
                0x20_u8 => {
                    let a = self.val.take();
                    self.val = a.as_pair().unwrap().0.clone();
                },
                // Rst
                0x21_u8 => {
                    let a = self.val.take();
                    self.val = a.as_pair().unwrap().1.clone();
                },
                // Cons
                0x22_u8 => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = Datum::make_pair(a, b);
                },
                // IsZero
                0x23_u8 => {
                    self.val = Datum::Bool(self.val.is_equal(&Datum::Integer(0)).unwrap());
                },
                // IsNil
                0x24_u8 => {
                    self.val = Datum::Bool(self.val == Datum::Nil);
                },
                // VectorRef
                0x25_u8 => {
                    let vector = self.checked_pop()?;
                    let index = self.val.take();

                    // TODO: Convert errors
                    let vector = vector.as_vector().unwrap();
                    match vector.get(index.as_uinteger().unwrap()) {
                        Some(e) => {
                            self.val = e.clone()
                        },
                        None => panic!("vector-ref index out of bounds")
                    }
                },
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
                },

                // Constant
                0x30_u8 => {
                    let i = self.fetch_u32()?;
                    self.val = self.constants[i as usize].clone();
                },
                // PushConstant
                0x31_u8 => {
                    let i = self.fetch_u32()?;
                    self.stack.push(self.constants[i as usize].clone());
                },
                // PushValue
                0x32_u8 => {
                    self.stack.push(self.val.take());
                },
                // PopFunction
                0x33_u8 => {
                    self.fun = self.checked_pop()?;
                },
                // PreserveEnv
                0x34_u8 => {
                    self.env_stack.push(self.env.clone());
                },
                // RestoreEnv
                0x35_u8 => {
                    if let Some(env) = self.env_stack.pop() {
                        self.env = env;
                    } else {
                        return Err(VMError::EnvStackUnderflow(self.pc));
                    }
                },
                // ExtendEnv
                0x36_u8 => {
                    let mut new_env = Env::new(Some(self.env.clone()));
                    new_env.extend(self.frame.clone());

                    self.env = Rc::new(RefCell::new(new_env));
                },
                // UnlinkEnv
                0x37_u8 => {
                    let parent = self.env.borrow().parent.clone().unwrap();
                    self.env = parent;
                },

                // CheckedGlobalRef
                0x40_u8 => {
                    let idx = self.fetch_u32()?;
                    let v = &self.global_env[idx as usize];
                    if *v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.val = v.clone();
                    }
                },
                // GlobalRef
                0x41_u8 => {
                    let idx = self.fetch_u32()?;
                    let v = &self.global_env[idx as usize];
                    self.val = v.clone();
                },
                // PushCheckedGlobalRef
                0x42_u8 => {
                    let idx = self.fetch_u32()?;
                    let v = &self.global_env[idx as usize];
                    if *v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.stack.push(v.clone());
                    }
                },
                // CheckedGlobalRef
                0x43_u8 => {
                    let idx = self.fetch_u32()?;
                    let v = &self.global_env[idx as usize];
                    self.stack.push(v.clone());
                },
                // GlobalSet
                0x44_u8 => {
                    let idx = self.fetch_u32()?;
                    self.global_env[idx as usize] = self.val.take();
                },

                // Call1
                0x50_u8 => {
                    let idx = self.fetch_u32()?;
                    let res = self.builtins.call_1(idx, self.val.take(), &self);
                    self.val = res.unwrap();
                },
                // Call2
                0x51_u8 => {
                    let idx = self.fetch_u32()?;
                    let arg1 = self.checked_pop()?;
                    let res = self.builtins.call_2(idx, arg1, self.val.take(), &self);
                    self.val = res.unwrap();
                },
                // Call3
                0x52_u8 => {
                    let idx = self.fetch_u32()?;
                    let arg2 = self.checked_pop()?;
                    let arg1 = self.checked_pop()?;
                    let res = self.builtins.call_3(idx, arg1, arg2, self.val.take(), &self);
                    self.val = res.unwrap();
                },
                // CallN
                0x53_u8 => {
                    let idx = self.fetch_u32()?;
                    let given = self.fetch_u8()? as usize;
                    let at = self.stack.len() - given;
                    let mut args = self.stack.split_off(at);
                    let res = self.builtins.call_n(idx, &mut args, &self);
                    self.val = res.unwrap()
                },

                0x60_u8 => {
                    let j = self.fetch_u16()?;
                    let env = self.env.borrow();
                    self.val = env.shallow_ref(j as usize);
                },
                0x61_u8 => {
                    let j = self.fetch_u16()?;
                    let env = self.env.borrow();
                    self.stack.push(env.shallow_ref(j as usize));
                },
                0x62_u8 => {
                    let i = self.fetch_u16()?;
                    let j = self.fetch_u16()?;
                    let env = self.env.borrow();
                    self.val = env.deep_ref(i as usize, j as usize);
                },
                0x63_u8 => {
                    let i = self.fetch_u16()?;
                    let j = self.fetch_u16()?;
                    let env = self.env.borrow();
                    self.stack.push(env.deep_ref(i as usize, j as usize));
                },
                0x64_u8 => {
                    let j = self.fetch_u16()?;
                    let mut env = self.env.borrow_mut();
                    env.shallow_set(j as usize, self.val.take());
                },
                0x65_u8 => {
                    let i = self.fetch_u16()?;
                    let j = self.fetch_u16()?;
                    let mut env = self.env.borrow_mut();
                    env.deep_set(i as usize, j as usize, self.val.take());
                },

                // TODO: Only decode offset if needed
                // Jump
                0x70_u8 => {
                    let offset = self.fetch_u32()?;
                    self.pc += offset as usize - 1;
                },
                // JumpTrue
                0x71_u8 => {
                    let offset = self.fetch_u32()?;
                    if self.val == Datum::Bool(true) {
                        self.pc += offset as usize - 1;
                    }
                },
                // JumpFalse
                0x72_u8 => {
                    let offset = self.fetch_u32()?;
                    if self.val == Datum::Bool(false) {
                        self.pc += offset as usize - 1;
                    }
                },
                // JumpNil
                0x73_u8 => {
                    let offset = self.fetch_u32()?;
                    if self.val == Datum::Nil {
                        self.pc += offset as usize - 1;
                    }
                },
                // JumpNotNil
                0x74_u8 => {
                    let offset = self.fetch_u32()?;
                    if self.val != Datum::Nil {
                        self.pc += offset as usize - 1;
                    }
                },
                // JumpZero
                0x75_u8 => {
                    let offset = self.fetch_u32()?;
                    if self.val.is_equal(&Datum::Integer(0)).unwrap() {
                        self.pc += offset as usize - 1;
                    }
                },
                // JumpNotZero
                0x76_u8 => {
                    let offset = self.fetch_u32()?;
                    if !self.val.is_equal(&Datum::Integer(0)).unwrap() {
                        self.pc += offset as usize - 1;
                    }
                },

                // FixClosure
                0x80_u8 => {
                    let offset = self.fetch_u16()?;
                    let arity = self.fetch_u16()?;

                    let closure = Datum::Closure(
                        self.pc + (offset as usize) + 1,
                        arity as usize,
                        false,
                        self.env.clone()
                        );
                    self.val = closure;
                },
                // DottedClosure
                0x81_u8 => {
                    let offset = self.fetch_u16()?;
                    let arity = self.fetch_u16()?;

                    let closure = Datum::Closure(
                        self.pc + (offset as usize) + 1,
                        arity as usize,
                        true,
                        self.env.clone()
                        );
                    self.val = closure;
                },
                // StoreArgument
                0x82_u8 => {
                    let idx = self.fetch_u32()?;
                    self.frame[idx as usize] = self.checked_pop()?;
                },
                // ConsArgument
                0x83_u8 => {
                    let idx = self.fetch_u32()?;
                    self.frame[idx as usize] = Datum::make_pair(
                        self.frame[idx as usize].take(),
                        self.val.take()
                    );
                },
                // AllocateFrame
                0x84_u8 => {
                    let size = self.fetch_u32()?;
                    self.frame = Vec::with_capacity(size as usize);
                    for _ in 0..size {
                        self.frame.push(Datum::Undefined);
                    }
                },
                // AllocateFillFrame
                0x85_u8 => {
                    let size = self.fetch_u32()?;
                    self.frame = Vec::with_capacity(size as usize);
                    for _ in 0..size {
                        let v = self.checked_pop()?;
                        self.frame.push(v);
                    }
                },
                // AllocateDottedFrame
                //
                // The same as `AllocateFrame`,
                // just sets the last element to '()
                // so that `ConsArgument` can add the dotted arguments to it
                0x86_u8 => {
                    let size = self.fetch_u32()?;
                    self.frame = Vec::with_capacity(size as usize);
                    for _ in 0..(size - 1) {
                        self.frame.push(Datum::Undefined);
                    }
                    self.frame.push(Datum::Nil);
                },
                // FunctionInvoke
                v @ 0x87_u8 | v @ 0x88_u8 => {
                    let is_tail = v == 0x88_u8;

                    // TODO: Include function symbol for debugging
                    if let Datum::Closure(offset, arity, dotted, ref env) = self.fun {
                        if !is_tail {
                            self.pc_stack.push(self.pc);
                        }

                        if dotted {
                            if (self.frame.len() + 1) < arity {
                                panic!("Incorrect arity");
                            }

                            let rest = self.frame.split_off(arity-1);
                            self.frame.push(Datum::make_list_from_vec(rest));

                            let mut new_env = Env::new(Some(env.clone()));
                            new_env.extend(self.frame.clone());

                            self.env = Rc::new(RefCell::new(new_env));
                            self.pc = offset - 1;
                        } else {
                            let got = self.frame.len();
                            if arity != self.frame.len() {
                                panic!("Incorrect arity, expected {}, got {}", arity, got);
                            }
                            let mut new_env = Env::new(Some(env.clone()));
                            new_env.extend(self.frame.clone());

                            self.env = Rc::new(RefCell::new(new_env));
                            self.pc = offset - 1;
                        }
                    } else if let Datum::Builtin(ref typ, index, ref arity) = self.fun {
                        arity.check(self.frame.len());
                        match typ {
                            LispFnType::Variadic => {
                                let mut args: Vec<Datum> =
                                    self.frame.iter_mut()
                                    .map(|d| d.take())
                                    .collect();
                                let res = self.builtins.call_n(index, &mut args, &self);
                                self.val = res.unwrap();
                            },
                            LispFnType::Fixed1 => {
                                let arg1 = self.frame[0].take();
                                let res = self.builtins.call_1(index, arg1, &self);
                                self.val = res.unwrap();
                            },
                            LispFnType::Fixed2 => {
                                let arg1 = self.frame[0].take();
                                let arg2 = self.frame[1].take();
                                let res = self.builtins.call_2(index, arg1, arg2, &self);
                                self.val = res.unwrap();
                            },
                            LispFnType::Fixed3 => {
                                let arg1 = self.frame[0].take();
                                let arg2 = self.frame[1].take();
                                let arg3 = self.frame[2].take();
                                let res = self.builtins.call_3(index, arg1, arg2, arg3, &self);
                                self.val = res.unwrap();
                            },
                        }
                    } else {
                        panic!("Trying to invoke non function {:?}", self.fun);
                    }
                },
                _ => unimplemented!()
            }

            // TODO: Another solution would be appending RETURN
            // to all programs, but that would make building a REPL harder
            if self.pc < self.program.len() - 1 {
                self.pc += 1;
            } else {
                break;
            }
        }

        Ok(())
    }


    pub fn run(&mut self) -> VMResult {
        loop {
            let inst = self.program[self.pc];
            match inst {
                Instruction::Finish => {
                    break;
                },
                Instruction::Inc => {
                    self.val = match self.val.take() {
                        Datum::Integer(x) => Datum::Integer(x + 1),
                        Datum::Float(x) => Datum::Float(x + 1.0),
                        Datum::Rational(x) => Datum::Rational(x + 1),
                        other => panic!("INC not implemented for {:?}", other)
                    }
                },
                Instruction::Dec => {
                    self.val = match self.val.take() {
                        Datum::Integer(x) => Datum::Integer(x - 1),
                        Datum::Float(x) => Datum::Float(x - 1.0),
                        Datum::Rational(x) => Datum::Rational(x - 1),
                        other => panic!("DEC not implemented for {:?}", other)
                    }
                },
                Instruction::Add => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a + b;
                },
                Instruction::Sub => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a - b;
                },
                Instruction::Mul => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a * b;
                },
                Instruction::Div => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a / b;
                },
                Instruction::Mod => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a % b;
                },
                Instruction::IntDiv => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = a.int_div(b);
                },
                Instruction::Fst => {
                    let a = self.val.take();
                    self.val = a.as_pair().unwrap().0.clone();
                },
                Instruction::Rst => {
                    let a = self.val.take();
                    self.val = a.as_pair().unwrap().1.clone();
                },
                Instruction::Cons => {
                    let a = self.checked_pop()?;
                    let b = self.val.take();
                    self.val = Datum::make_pair(a, b);
                },
                Instruction::Equal => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.is_equal(&self.val).unwrap());
                },
                Instruction::Eq => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a == self.val);
                },
                Instruction::Neq => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a != self.val);
                },
                Instruction::Not => {
                    self.val = Datum::Bool(self.val == Datum::Bool(false));
                },
                Instruction::Lt => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() == Ordering::Less);
                },
                Instruction::Gt => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() == Ordering::Greater);
                },
                Instruction::Lte => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() != Ordering::Greater);
                },
                Instruction::Gte => {
                    let a = self.checked_pop()?;
                    self.val = Datum::Bool(a.compare(&self.val).unwrap() != Ordering::Less);
                },
                Instruction::IsZero => {
                    self.val = Datum::Bool(self.val.is_equal(&Datum::Integer(0)).unwrap());
                },
                Instruction::IsNil => {
                    self.val = Datum::Bool(self.val == Datum::Nil);
                },
                Instruction::VectorRef => {
                    let vector = self.checked_pop()?;
                    let index = self.val.take();

                    // TODO: Convert errors
                    let vector = vector.as_vector().unwrap();
                    match vector.get(index.as_uinteger().unwrap()) {
                        Some(e) => {
                            self.val = e.clone()
                        },
                        None => panic!("vector-ref index out of bounds")
                    }
                },
                Instruction::VectorSet => {
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
                },
                Instruction::PushValue => {
                    self.stack.push(self.val.take());
                },
                Instruction::PopFunction => {
                    self.fun = self.checked_pop()?;
                },
                Instruction::FunctionInvoke(is_tail) => {
                    // TODO: Include function symbol for debugging
                    if let Datum::Closure(offset, arity, dotted, ref env) = self.fun {
                        if !is_tail {
                            self.pc_stack.push(self.pc);
                        }

                        if dotted {
                            if (self.frame.len() + 1) < arity {
                                panic!("Incorrect arity");
                            }

                            let rest = self.frame.split_off(arity-1);
                            self.frame.push(Datum::make_list_from_vec(rest));

                            let mut new_env = Env::new(Some(env.clone()));
                            new_env.extend(self.frame.clone());

                            self.env = Rc::new(RefCell::new(new_env));
                            self.pc = offset - 1;
                        } else {
                            let got = self.frame.len();
                            if arity != self.frame.len() {
                                panic!("Incorrect arity, expected {}, got {}", arity, got);
                            }
                            let mut new_env = Env::new(Some(env.clone()));
                            new_env.extend(self.frame.clone());

                            self.env = Rc::new(RefCell::new(new_env));
                            self.pc = offset - 1;
                        }
                    } else if let Datum::Builtin(ref typ, index, ref arity) = self.fun {
                        arity.check(self.frame.len());
                        match typ {
                            LispFnType::Variadic => {
                                let mut args: Vec<Datum> =
                                    self.frame.iter_mut()
                                    .map(|d| d.take())
                                    .collect();
                                let res = self.builtins.call_n(index, &mut args, &self);
                                self.val = res.unwrap();
                            },
                            LispFnType::Fixed1 => {
                                let arg1 = self.frame[0].take();
                                let res = self.builtins.call_1(index, arg1, &self);
                                self.val = res.unwrap();
                            },
                            LispFnType::Fixed2 => {
                                let arg1 = self.frame[0].take();
                                let arg2 = self.frame[1].take();
                                let res = self.builtins.call_2(index, arg1, arg2, &self);
                                self.val = res.unwrap();
                            },
                            LispFnType::Fixed3 => {
                                let arg1 = self.frame[0].take();
                                let arg2 = self.frame[1].take();
                                let arg3 = self.frame[2].take();
                                let res = self.builtins.call_3(index, arg1, arg2, arg3, &self);
                                self.val = res.unwrap();
                            },
                        }
                    } else {
                        panic!("Trying to invoke non function {:?}", self.fun);
                    }
                },
                Instruction::PreserveEnv => {
                    self.env_stack.push(self.env.clone());
                },
                Instruction::RestoreEnv => {
                    if let Some(env) = self.env_stack.pop() {
                        self.env = env;
                    } else {
                        return Err(VMError::EnvStackUnderflow(self.pc));
                    }
                },
                Instruction::ExtendEnv => {
                    let mut new_env = Env::new(Some(self.env.clone()));
                    new_env.extend(self.frame.clone());

                    self.env = Rc::new(RefCell::new(new_env));
                },
                Instruction::UnlinkEnv => {
                    let parent = self.env.borrow().parent.clone().unwrap();
                    self.env = parent;
                },
                Instruction::Constant(i) => {
                    self.val = self.constants[i as usize].clone();
                },
                Instruction::PushConstant(i) => {
                    self.stack.push(self.constants[i as usize].clone());
                },
                Instruction::Call1(index) => {
                    let res = self.builtins.call_1(index, self.val.take(), &self);
                    self.val = res.unwrap();
                },
                Instruction::Call2(index) => {
                    let arg1 = self.checked_pop()?;
                    let res = self.builtins.call_2(index, arg1, self.val.take(), &self);
                    self.val = res.unwrap();
                },
                Instruction::Call3(index) => {
                    let arg2 = self.checked_pop()?;
                    let arg1 = self.checked_pop()?;
                    let res = self.builtins.call_3(index, arg1, arg2, self.val.take(), &self);
                    self.val = res.unwrap();
                },
                Instruction::CallN(index, given) => {
                    let given = given as usize;
                    let at = self.stack.len() - given;
                    let mut args = self.stack.split_off(at);
                    let res = self.builtins.call_n(index, &mut args, &self);
                    self.val = res.unwrap()
                },
                Instruction::GlobalSet(idx) => {
                    self.global_env[idx as usize] = self.val.take();
                },
                Instruction::CheckedGlobalRef(idx) => {
                    let v = &self.global_env[idx as usize];
                    if *v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.val = v.clone();
                    }
                },
                Instruction::PushCheckedGlobalRef(idx) => {
                    let v = &self.global_env[idx as usize];
                    if *v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.stack.push(v.clone());
                    }
                },
                Instruction::GlobalRef(idx) => {
                    let v = &self.global_env[idx as usize];
                    self.val = v.clone();
                },
                Instruction::PushGlobalRef(idx) => {
                    let v = &self.global_env[idx as usize];
                    self.stack.push(v.clone());
                },
                Instruction::Jump(offset) => {
                    self.pc += offset as usize - 1;
                },
                Instruction::JumpNotNil(offset) => {
                    if self.val != Datum::Nil {
                        self.pc += offset as usize - 1;
                    }
                },
                Instruction::JumpNil(offset) => {
                    if self.val == Datum::Nil {
                        self.pc += offset as usize - 1;
                    }
                },
                Instruction::JumpTrue(offset) => {
                    if self.val == Datum::Bool(true) {
                        self.pc += offset as usize - 1;
                    }
                },
                Instruction::JumpFalse(offset) => {
                    if self.val == Datum::Bool(false) {
                        self.pc += offset as usize - 1;
                    }
                },
                Instruction::JumpZero(offset) => {
                    if self.val.is_equal(&Datum::Integer(0)).unwrap() {
                        self.pc += offset as usize - 1;
                    }
                },
                Instruction::JumpNotZero(offset) => {
                    if !self.val.is_equal(&Datum::Integer(0)).unwrap() {
                        self.pc += offset as usize - 1;
                    }
                },
                Instruction::FixClosure(offset, arity) => {
                    let closure = Datum::Closure(
                        self.pc + (offset as usize) + 1,
                        arity as usize,
                        false,
                        self.env.clone()
                        );
                    self.val = closure;
                },
                Instruction::DottedClosure(offset, arity) => {
                    let closure = Datum::Closure(
                        self.pc + (offset as usize) + 1,
                        arity as usize,
                        true,
                        self.env.clone()
                        );
                    self.val = closure;
                },
                Instruction::Return => {
                    if let Some(pc) = self.pc_stack.pop() {
                        self.pc = pc;
                    } else {
                        return Err(VMError::PCStackUnderflow(self.pc));
                    }
                },
                Instruction::AllocateFrame(size) => {
                    self.frame = Vec::with_capacity(size as usize);
                    for _ in 0..size {
                        self.frame.push(Datum::Undefined);
                    }
                },
                Instruction::AllocateFillFrame(size) => {
                    self.frame = Vec::with_capacity(size as usize);
                    for _ in 0..size {
                        let v = self.checked_pop()?;
                        self.frame.push(v);
                    }
                },
                // The same as `AllocateFrame`,
                // just sets the last element to '()
                // so that `ConsArgument` can add the dotted arguments to it
                Instruction::AllocateDottedFrame(size) => {
                    self.frame = Vec::with_capacity(size as usize);
                    for _ in 0..(size - 1) {
                        self.frame.push(Datum::Undefined);
                    }
                    self.frame.push(Datum::Nil);
                },
                Instruction::StoreArgument(idx) => {
                    self.frame[idx as usize] = self.checked_pop()?;
                },
                Instruction::ConsArgument(arity) => {
                    self.frame[arity as usize] = Datum::make_pair(
                        self.frame[arity as usize].take(),
                        self.val.take()
                        );
                },
                Instruction::ShallowArgumentRef(j) => {
                    let env = self.env.borrow();
                    self.val = env.shallow_ref(j as usize);
                },
                Instruction::PushShallowArgumentRef(j) => {
                    let env = self.env.borrow();
                    self.stack.push(env.shallow_ref(j as usize));
                },
                Instruction::DeepArgumentRef(i, j) => {
                    let env = self.env.borrow();
                    self.val = env.deep_ref(i as usize, j as usize);
                },
                Instruction::PushDeepArgumentRef(i, j) => {
                    let env = self.env.borrow();
                    self.stack.push(env.deep_ref(i as usize, j as usize));
                },
                Instruction::ShallowArgumentSet(j) => {
                    let mut env = self.env.borrow_mut();
                    env.shallow_set(j as usize, self.val.take());
                },
                Instruction::DeepArgumentSet(i, j) => {
                    let mut env = self.env.borrow_mut();
                    env.deep_set(i as usize, j as usize, self.val.take());
                },
            }

            // TODO: Another solution would be appending RETURN
            // to all programs, but that would make building a REPL harder
            if self.pc < self.program.len() - 1 {
                self.pc += 1;
            } else {
                break;
            }
        }
        Ok(())
    }
}
