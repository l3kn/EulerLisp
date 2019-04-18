use std::cell::RefCell;
use std::cmp::Ordering;
use std::convert::TryInto;
use std::io::Write;
use std::rc::Rc;

use crate::builtin::BuiltinRegistry;
use crate::env::{Env, EnvRef};
use crate::symbol_table::SymbolTable;
use crate::{IntegerDiv, LispFnType, Value};

mod bytecode;
mod error;

use bytecode::Bytecode;
use error::Error as VMError;
use error::Result as VMResult;

pub struct VM {
    pub val: Value,
    pub arg1: Value,
    pub arg2: Value,
    fun: Value,
    env: EnvRef,
    stack: Vec<Value>,
    env_stack: Vec<EnvRef>,
    pc_stack: Vec<usize>,
    global_env: Vec<Value>,
    pub bytecode: Bytecode,
    pub output: Rc<RefCell<Write>>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Value>,
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
            bytecode: Bytecode::new(vec![0x01_u8], 1),
            symbol_table,
            builtins,
            output,
            global_env: Vec::new(),
            val: Value::Undefined,
            arg1: Value::Undefined,
            arg2: Value::Undefined,
            fun: Value::Undefined,
            env: Rc::new(RefCell::new(local_env)),
            stack,
            env_stack: Vec::new(),
            pc_stack: vec![0],
            constants: Vec::new(),
        }
    }

    pub fn set_pc(&mut self, v: usize) {
        self.bytecode.pc = v;
    }

    pub fn add_global(&mut self, g: Value) {
        self.global_env.push(g);
    }

    pub fn append_instructions(&mut self, insts: Vec<u8>) {
        self.bytecode.extend(insts);
    }

    pub fn append_constants(&mut self, consts: Vec<Value>) {
        self.constants.extend(consts);
    }

    pub fn reserve_global_vars(&mut self, count: usize) {
        self.global_env.reserve(count);
        for _ in 0..count {
            self.global_env.push(Value::Undefined);
        }
    }

    fn checked_pop(&mut self) -> Result<Value, VMError> {
        if let Some(dat) = self.stack.pop() {
            Ok(dat)
        } else {
            Err(VMError::StackUnderflow(self.bytecode.pc))
        }
    }

    fn seek_current(&mut self, offset: u32) {
        self.bytecode.pc += offset as usize;
    }

    fn seek_start(&mut self, offset: usize) {
        self.bytecode.pc = offset as usize;
    }

    fn preserve_env(&mut self) {
        self.env_stack.push(self.env.clone());
    }

    pub fn run(&mut self) -> VMResult {
        let end = self.bytecode.len();
        while self.bytecode.pc != end {
            // TODO: Propagate errors
            let inst = self.bytecode.fetch_u8();
            match inst {
                // Return
                0x00_u8 => {
                    if let Some(pc) = self.pc_stack.pop() {
                        self.seek_start(pc);
                    } else {
                        return Err(VMError::PCStackUnderflow(self.bytecode.pc));
                    }
                }
                // Finish
                0x01_u8 => break,
                // Inc
                0x10_u8 => {
                    self.val = match self.val.take() {
                        Value::Integer(x) => Value::Integer(x + 1),
                        Value::Float(x) => Value::Float(x + 1.0),
                        Value::Rational(x) => Value::Rational(x + 1),
                        Value::Bignum(x) => Value::Bignum(x + 1),
                        other => panic!("INC not implemented for {:?}", other),
                    }
                }
                // Dec
                0x11_u8 => {
                    self.val = match self.val.take() {
                        Value::Integer(x) => Value::Integer(x - 1),
                        Value::Float(x) => Value::Float(x - 1.0),
                        Value::Rational(x) => Value::Rational(x - 1),
                        Value::Bignum(x) => Value::Bignum(x - 1),
                        other => panic!("DEC not implemented for {:?}", other),
                    }
                }
                // Add
                0x12_u8 => self.val = self.val.take() + self.arg1.take(),
                // Sub
                0x13_u8 => self.val = self.val.take() - self.arg1.take(),
                // Mul
                0x14_u8 => self.val = self.val.take() * self.arg1.take(),
                // Div
                0x15_u8 => self.val = self.val.take() / self.arg1.take(),
                // Mod
                0x16_u8 => self.val = self.val.take() % self.arg1.take(),
                // IntDiv
                0x17_u8 => {
                    let a = self.val.take();
                    let b = self.arg1.take();
                    self.val = a.int_div(b);
                }

                // Not
                0x18_u8 => {
                    self.val = Value::Bool(self.val == Value::Bool(false));
                }
                // Equal
                0x19_u8 => {
                    self.val = Value::Bool(self.arg1.is_equal(&self.val).unwrap());
                }
                // Eq
                0x1A_u8 => {
                    self.val = Value::Bool(self.arg1 == self.val);
                }
                // Neq
                0x1B_u8 => {
                    self.val = Value::Bool(self.arg1 != self.val);
                }
                // Gt
                0x1C_u8 => {
                    let a = self.val.take();
                    self.val = Value::Bool(a.compare(&self.arg1).unwrap() == Ordering::Greater);
                }
                // Gte
                0x1D_u8 => {
                    let a = self.val.take();
                    self.val = Value::Bool(a.compare(&self.arg1).unwrap() != Ordering::Less);
                }
                // Lt
                0x1E_u8 => {
                    let a = self.val.take();
                    self.val = Value::Bool(a.compare(&self.arg1).unwrap() == Ordering::Less);
                }
                // Lte
                0x1F_u8 => {
                    let a = self.val.take();
                    self.val = Value::Bool(a.compare(&self.arg1).unwrap() != Ordering::Greater);
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
                    let a = self.val.take();
                    let b = self.arg1.take();
                    self.val = Value::make_pair(a, b);
                }
                // IsZero
                0x23_u8 => {
                    self.val = Value::Bool(self.val.is_equal(&Value::Integer(0)).unwrap());
                }
                // IsNil
                0x24_u8 => self.val = Value::Bool(self.val == Value::Nil),
                // VectorRef
                0x25_u8 => {
                    let vector = self.val.take();
                    let vector = vector.as_vector().unwrap();
                    let index: usize = self.arg1.take().try_into().unwrap();

                    // TODO: Convert errors
                    match vector.get(index) {
                        Some(e) => self.val = e.clone(),
                        None => panic!("vector-ref index out of bounds"),
                    }
                }
                // VectorSet
                0x26_u8 => {
                    let mut vector = self.val.as_mut_vector().unwrap();
                    let index: usize = self.arg1.take().try_into().unwrap();

                    if index < vector.len() {
                        vector[index] = self.arg2.take();
                    } else {
                        panic!("vector-set index out of bounds")
                    }
                }

                // Constant
                0x30_u8 => {
                    let i = self.bytecode.fetch_u16_as_usize();
                    self.val = self.constants[i].clone();
                }
                // PushConstant
                0x31_u8 => {
                    let i = self.bytecode.fetch_u16_as_usize();
                    self.stack.push(self.constants[i].clone());
                }
                // PushValue
                0x32_u8 => self.stack.push(self.val.take()),
                // PopFunction
                0x33_u8 => self.fun = self.checked_pop()?,
                // PopArg1
                0x34_u8 => self.arg1 = self.checked_pop()?,
                // PopArg2
                0x35_u8 => self.arg2 = self.checked_pop()?,
                // PreserveEnv
                0x36_u8 => self.preserve_env(),
                // RestoreEnv
                0x37_u8 => {
                    if let Some(env) = self.env_stack.pop() {
                        self.env = env;
                    } else {
                        return Err(VMError::EnvStackUnderflow(self.bytecode.pc));
                    }
                }
                // ExtendEnv
                0x38_u8 => {
                    let mut new_env = Env::new(Some(self.env.clone()));
                    if let Value::ActivationFrame(elems) = self.val.take() {
                        new_env.extend(elems);
                    } else {
                        panic!("ExtendEnv without a activation frame in val");
                    }

                    self.env = Rc::new(RefCell::new(new_env));
                }
                // UnlinkEnv
                0x39_u8 => {
                    let parent = self.env.borrow().parent.clone().unwrap();
                    self.env = parent;
                }

                // CheckedGlobalRef
                0x40_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let v = &self.global_env[idx];
                    if *v == Value::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.val = v.clone();
                    }
                }
                // GlobalRef
                0x41_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let v = &self.global_env[idx];
                    self.val = v.clone();
                }
                // PushCheckedGlobalRef
                0x42_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let v = &self.global_env[idx];
                    if *v == Value::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.stack.push(v.clone());
                    }
                }
                // CheckedGlobalRef
                0x43_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let v = &self.global_env[idx];
                    self.stack.push(v.clone());
                }
                // GlobalSet
                0x44_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    self.global_env[idx] = self.val.take();
                }

                // Call1
                0x50_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let res = self.builtins.call_1(idx, self.val.take(), &self);
                    self.val = res.unwrap();
                }
                // Call2
                0x51_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let res = self
                        .builtins
                        .call_2(idx, self.val.take(), self.arg1.take(), &self);
                    self.val = res.unwrap();
                }
                // Call3
                0x52_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let res = self.builtins.call_3(
                        idx,
                        self.val.take(),
                        self.arg1.take(),
                        self.arg2.take(),
                        &self,
                    );
                    self.val = res.unwrap();
                }
                // CallN
                0x53_u8 => {
                    let idx = self.bytecode.fetch_u16_as_usize();
                    let given = self.bytecode.fetch_u8() as usize;
                    let at = self.stack.len() - given;
                    let mut args = self.stack.split_off(at);
                    let res = self.builtins.call_n(idx, &mut args, &self);
                    self.val = res.unwrap()
                }

                // ShallowArgumentRef
                0x60_u8 => {
                    let j = self.bytecode.fetch_u16_as_usize();
                    let env = self.env.borrow();
                    self.val = env.shallow_ref(j);
                }
                // PushShallowArgumentRef
                0x61_u8 => {
                    let j = self.bytecode.fetch_u16_as_usize();
                    let env = self.env.borrow();
                    self.stack.push(env.shallow_ref(j));
                }
                // ShallowArgumentSet
                0x62_u8 => {
                    let j = self.bytecode.fetch_u16_as_usize();
                    let mut env = self.env.borrow_mut();
                    env.shallow_set(j, self.val.take());
                }
                // DeepArgumentRef
                0x63_u8 => {
                    let i = self.bytecode.fetch_u16_as_usize();
                    let j = self.bytecode.fetch_u16_as_usize();
                    let env = self.env.borrow();
                    self.val = env.deep_ref(i, j);
                }
                // PushDeepArgumentRef
                0x64_u8 => {
                    let i = self.bytecode.fetch_u16_as_usize();
                    let j = self.bytecode.fetch_u16_as_usize();
                    let env = self.env.borrow();
                    self.stack.push(env.deep_ref(i, j));
                }
                // DeepArgumentSet
                0x65_u8 => {
                    let i = self.bytecode.fetch_u16_as_usize();
                    let j = self.bytecode.fetch_u16_as_usize();
                    let mut env = self.env.borrow_mut();
                    env.deep_set(i, j, self.val.take());
                }

                // Jump
                0x70_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    self.seek_current(offset);
                }
                // JumpTrue
                0x71_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    if self.val.is_true() {
                        self.seek_current(offset);
                    }
                }
                // JumpFalse
                0x72_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    if self.val.is_false() {
                        self.seek_current(offset);
                    }
                }
                // JumpNil
                0x73_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    if self.val == Value::Nil {
                        self.seek_current(offset);
                    }
                }
                // JumpNotNil
                0x74_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    if self.val != Value::Nil {
                        self.seek_current(offset);
                    }
                }
                // JumpZero
                0x75_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    if self.val.is_equal(&Value::Integer(0)).unwrap() {
                        self.seek_current(offset);
                    }
                }
                // JumpNotZero
                0x76_u8 => {
                    let offset = self.bytecode.fetch_u32();
                    if !self.val.is_equal(&Value::Integer(0)).unwrap() {
                        self.seek_current(offset);
                    }
                }

                // FixClosure
                0x80_u8 => {
                    let arity = self.bytecode.fetch_u16_as_usize();

                    // The next instruction, a jump behind the closure,
                    // needs to be scipped (1 byte type + 4 bytes addr)
                    let closure =
                        Value::Closure(self.bytecode.pc + 5, arity, false, self.env.clone());
                    self.val = closure;
                }
                // DottedClosure
                0x81_u8 => {
                    let arity = self.bytecode.fetch_u16_as_usize();

                    let closure =
                        Value::Closure(self.bytecode.pc + 5, arity, true, self.env.clone());
                    self.val = closure;
                }
                // StoreArgument
                0x82_u8 => {
                    unimplemented!();
                    // let idx = self.bytecode.fetch_u8_as_usize();
                    // self.frame[idx] = self.checked_pop()?;
                }
                // ConsArgument
                0x83_u8 => {
                    unimplemented!();
                    // let idx = self.bytecode.fetch_u8_as_usize();
                    // self.frame[idx] = Value::make_pair(self.frame[idx].take(), self.val.take());
                }
                // AllocateFrame
                0x84_u8 => {
                    unimplemented!();
                    // let size = self.bytecode.fetch_u8_as_usize();
                    // self.frame = Vec::with_capacity(size);
                    // for _ in 0..size {
                    //     self.frame.push(Value::Undefined);
                    // }
                }
                // AllocateFillFrame
                0x85_u8 => {
                    let size = self.bytecode.fetch_u8_as_usize();
                    let mut frame = Vec::with_capacity(size);
                    // self.frame = Vec::with_capacity(size);
                    for _ in 0..size {
                        let v = self.checked_pop()?;
                        frame.push(v);
                        // self.frame.push(v);
                    }
                    self.val = Value::ActivationFrame(frame);
                }
                // AllocateDottedFrame
                //
                // The same as `AllocateFrame`,
                // just sets the last element to '()
                // so that `ConsArgument` can add the dotted arguments to it
                0x86_u8 => {
                    unimplemented!();
                    // let size = self.bytecode.fetch_u8_as_usize();
                    // self.frame = Vec::with_capacity(size);
                    // for _ in 0..(size - 1) {
                    //     self.frame.push(Value::Undefined);
                    // }
                    // self.frame.push(Value::Nil);
                }
                // FunctionInvoke
                // TODO: Include function symbol for debugging
                v @ 0x87_u8 | v @ 0x88_u8 => {
                    let is_tail = v == 0x88_u8;
                    if !is_tail {
                        self.preserve_env();
                    }

                    // Must be constructed first,
                    // because the arguments are on top of the function
                    // on the stack
                    let size = self.bytecode.fetch_u8_as_usize();
                    let mut elems = Vec::with_capacity(size);
                    for _ in 0..size {
                        elems.push(self.checked_pop()?);
                    }

                    // The function is the topmost element on the stack
                    match self.checked_pop()? {
                        Value::Closure(offset, arity, dotted, ref env) => {
                            if !is_tail {
                                self.pc_stack.push(self.bytecode.pc);
                            }
                            let mut new_env = Env::new(Some(env.clone()));
                            if dotted {
                                if (size + 1) < arity {
                                    panic!("Incorrect arity");
                                }

                                let rest = elems.split_off(arity - 1);
                                elems.push(Value::make_list_from_vec(rest));
                            } else if arity != size {
                                panic!("Incorrect arity, expected {}, got {}", arity, size);
                            }
                            new_env.extend(elems);
                            self.env = Rc::new(RefCell::new(new_env));
                            self.bytecode.pc = offset;
                        }
                        Value::Builtin(ref typ, idx, ref arity) => {
                            let idx = idx as usize;
                            arity.check(size);

                            match typ {
                                LispFnType::Variadic => {
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
                        }
                        other => panic!("Trying to invoke non function {:?}", other),
                    }
                }
                _ => unimplemented!(),
            }
        }

        Ok(())
    }
}
