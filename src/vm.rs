use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::io::Write;
use std::rc::Rc;

use crate::{Datum, Expression, IntegerDiv, LispFnType, LispResult};
use crate::builtin::{self, BuiltinRegistry};
use crate::env::Env;
use crate::heap::{Heap, EnvRef};
use crate::compiler::{Compiler, Program};
use crate::parser::Parser;
use crate::instruction::convert_instructions;

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

pub type OutputRef = Rc<RefCell<Write>>;
pub struct VM {
    pub val: Datum,
    pub arg1: Datum,
    pub arg2: Datum,
    fun: Datum,
    env: EnvRef,
    stack: Vec<Datum>,
    env_stack: Vec<EnvRef>,
    pc_stack: Vec<usize>,
    global_env: Vec<Datum>,
    pub bytecode: Vec<u8>,
    pub output: OutputRef,
    pub compiler: Compiler,
    pc: usize,
    constants: Vec<Datum>,
    builtins: BuiltinRegistry,
    pub heap: Heap,
}

impl VM {
    pub fn new(output: OutputRef) -> VM {
        let stack = Vec::with_capacity(1000);
        let local_env = Env::new(None);

        let mut builtins = BuiltinRegistry::new();
        builtin::load(&mut builtins);

        let compiler = Compiler::new(builtins.clone());

        // Start with one "Finish" instruction,
        // the pc pointing behind it and an empty pc stack.
        // This way the last return (e.g. from a tail optimized function)
        // ends the execution.
        let mut heap = Heap::new();
        let local_env_ref = heap.insert_env(local_env);
        VM {
            compiler,
            bytecode: vec![0x01_u8],
            builtins,
            output,
            global_env: Vec::new(),
            val: Datum::Undefined,
            arg1: Datum::Undefined,
            arg2: Datum::Undefined,
            fun: Datum::Undefined,
            env: local_env_ref,
            stack,
            env_stack: Vec::new(),
            pc_stack: vec![0],
            pc: 1,
            constants: Vec::new(),
            heap,
        }
    }

    pub fn load_str(&mut self, input: &str, tail: bool, source: Option<String>) {
        let string = String::from(input);
        let mut parser = Parser::from_string(&string, source);

        // TODO: convert parser errors to lisp errors
        let mut datums: Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        let Program {
            instructions,
            constants,
            num_globals,
        } = self.compiler.compile(datums, tail);

        let constants: Vec<Datum> =
            constants.into_iter().map(|c| c.to_datum(
                &mut self.compiler.symbol_table,
                &mut self.heap
            )).collect();

        self.append_instructions(convert_instructions(instructions));
        self.append_constants(constants);
        self.reserve_global_vars(num_globals);
    }

    pub fn set_pc(&mut self, v: usize) {
        self.pc = v;
    }

    pub fn bind_global(&mut self, name: &str, val: Datum) {
        self.compiler.bind_global(name);
        self.global_env.push(val);
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
        let mut res = u32::from(self.bytecode[self.pc]);
        res += u32::from(self.bytecode[self.pc + 1]) << 8;
        res += u32::from(self.bytecode[self.pc + 2]) << 8;
        res += u32::from(self.bytecode[self.pc + 3]) << 8;
        self.pc += 4;
        res
    }

    // fn fetch_u32_usize(&mut self) -> usize {
    //     let mut res = usize::from(self.bytecode[self.pc]);
    //     res += usize::from(self.bytecode[self.pc + 1]) << 8;
    //     res += usize::from(self.bytecode[self.pc + 2]) << 8;
    //     res += usize::from(self.bytecode[self.pc + 3]) << 8;
    //     self.pc += 4;
    //     res
    // }

    // fn fetch_u16(&mut self) -> u16 {
    //     let mut res = u16::from(self.bytecode[self.pc]);
    //     res += u16::from(self.bytecode[self.pc + 1]) << 8;
    //     self.pc += 2;
    //     res
    // }

    fn fetch_u16_usize(&mut self) -> usize {
        let mut res = usize::from(self.bytecode[self.pc]);
        res += usize::from(self.bytecode[self.pc + 1]) << 8;
        self.pc += 2;
        res
    }

    fn fetch_u8(&mut self) -> u8 {
        let res = self.bytecode[self.pc];
        self.pc += 1;
        res
    }

    fn fetch_u8_usize(&mut self) -> usize {
        let res = self.bytecode[self.pc];
        self.pc += 1;
        res as usize
    }

    fn seek_current(&mut self, offset: u32) {
        self.pc += offset as usize;
    }

    fn seek_start(&mut self, offset: usize) {
        self.pc = offset as usize;
    }

    fn preserve_env(&mut self) {
        self.env_stack.push(self.env);
    }

    fn call_1(&mut self, idx: usize, arg1: Datum) -> LispResult<Datum> {
        self.builtins.fns_1[idx](arg1, &self.output, &mut self.compiler.symbol_table, &mut self.heap)
    }

    fn call_2(&mut self, idx: usize, arg1: Datum, arg2: Datum) -> LispResult<Datum> {
        self.builtins.fns_2[idx](arg1, arg2, &self.output, &mut self.compiler.symbol_table, &mut self.heap)
    }

    fn call_3(&mut self, idx: usize, arg1: Datum, arg2: Datum, arg3: Datum) -> LispResult<Datum> {
        self.builtins.fns_3[idx](arg1, arg2, arg3, &self.output, &mut self.compiler.symbol_table, &mut self.heap)
    }

    fn call_n(&mut self, idx: usize, args: &mut[Datum]) -> LispResult<Datum> {
        self.builtins.fns_n[idx](args, &self.output, &mut self.compiler.symbol_table, &mut self.heap)
    }

    pub fn run(&mut self) -> VMResult {
        let end = self.bytecode.len();
        while self.pc != end {
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
                    self.val = match self.val {
                        Datum::Integer(x) => Datum::Integer(x + 1),
                        Datum::Float(x) => Datum::Float(x + 1.0),
                        Datum::Rational(x) => Datum::Rational(x + 1),
                        other => panic!("INC not implemented for {:?}", other),
                    }
                }
                // Dec
                0x11_u8 => {
                    self.val = match self.val {
                        Datum::Integer(x) => Datum::Integer(x - 1),
                        Datum::Float(x) => Datum::Float(x - 1.0),
                        Datum::Rational(x) => Datum::Rational(x - 1),
                        other => panic!("DEC not implemented for {:?}", other),
                    }
                }
                // Add
                0x12_u8 => self.val = self.val.add(self.arg1, &mut self.heap),
                // Sub
                0x13_u8 => self.val = self.val - self.arg1,
                // Mul
                0x14_u8 => self.val = self.val.mult(self.arg1, &mut self.heap),
                // Div
                0x15_u8 => self.val = self.val / self.arg1,
                // Mod
                0x16_u8 => self.val = self.val % self.arg1,
                // IntDiv
                0x17_u8 => {
                    let a = self.val;
                    let b = self.arg1;
                    self.val = a.int_div(b);
                }

                // Not
                0x18_u8 => {
                    self.val = Datum::Bool(self.val == Datum::Bool(false));
                }
                // Equal
                0x19_u8 => {
                    self.val = Datum::Bool(self.arg1.is_equal(&self.val, &self.heap).unwrap());
                }
                // Eq
                0x1A_u8 => {
                    self.val = Datum::Bool(self.arg1 == self.val);
                }
                // Neq
                0x1B_u8 => {
                    self.val = Datum::Bool(self.arg1 != self.val);
                }
                // Gt
                0x1C_u8 => {
                    let a = self.val;
                    self.val = Datum::Bool(a.compare(&self.arg1, &self.heap).unwrap() == Ordering::Greater);
                }
                // Gte
                0x1D_u8 => {
                    let a = self.val;
                    self.val = Datum::Bool(a.compare(&self.arg1, &self.heap).unwrap() != Ordering::Less);
                }
                // Lt
                0x1E_u8 => {
                    let a = self.val;
                    self.val = Datum::Bool(a.compare(&self.arg1, &self.heap).unwrap() == Ordering::Less);
                }
                // Lte
                0x1F_u8 => {
                    let a = self.val;
                    self.val = Datum::Bool(a.compare(&self.arg1, &self.heap).unwrap() != Ordering::Greater);
                }

                // Fst
                0x20_u8 => {
                    self.val =
                        if let Datum::Pair(pair_ref) = self.val {
                            let pair = self.heap.get_pair(pair_ref);
                            pair.0
                        } else {
                            // FIXME
                            panic!("fst of non-pair");
                            // Err(LispErr::TypeError("convert", "pair", self.val.clone()))?
                        };
                }
                // Rst
                0x21_u8 => {
                    self.val =
                        if let Datum::Pair(pair_ref) = self.val {
                            let pair = self.heap.get_pair(pair_ref);
                            pair.1
                        } else {
                            // FIXME
                            panic!("rst of non-pair");
                            // Err(LispErr::TypeError("convert", "pair", self.val.clone()))?
                        };
                }
                // Cons
                0x22_u8 => {
                    let a = self.val;
                    let b = self.arg1;
                    self.val = self.heap.make_pair(a, b);
                }
                // IsZero
                0x23_u8 => {
                    self.val = Datum::Bool(self.val.is_equal(&Datum::Integer(0), &self.heap).unwrap());
                }
                // IsNil
                0x24_u8 => self.val = Datum::Bool(self.val == Datum::Nil),
                // VectorRef
                0x25_u8 => {
                    let index = self.arg1.as_uinteger().unwrap();
                    if let Datum::Vector(ptr) = self.val {
                        self.val = self.heap.get_vector(ptr)[index];
                    } else {
                        panic!("vector-ref argument not a vector");
                    };
                }
                // VectorSet
                0x26_u8 => {
                    let index = self.arg1.as_uinteger().unwrap();
                    if let Datum::Vector(ptr) = self.val {
                        self.heap.get_vector_mut(ptr)[index] = self.arg2;
                    } else {
                        panic!("vector-ref argument not a vector");
                    };
                }
                // Constant
                0x30_u8 => {
                    let i = self.fetch_u16_usize();
                    self.val = self.constants[i];
                }
                // PushConstant
                0x31_u8 => {
                    let i = self.fetch_u16_usize();
                    self.stack.push(self.constants[i]);
                }
                // PushValue
                0x32_u8 => self.stack.push(self.val),
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
                        return Err(VMError::EnvStackUnderflow(self.pc));
                    }
                }
                // ExtendEnv
                0x38_u8 => {
                    let mut new_env = Env::new(Some(self.env));
                    if let Datum::ActivationFrame(elems) = self.val {
                        let elems = self.heap.get_activation_frame(elems);
                        new_env.extend(elems.clone());
                    } else {
                        panic!("ExtendEnv without a activation frame in val");
                    }

                    self.env = self.heap.insert_env(new_env);
                }
                // UnlinkEnv
                0x39_u8 => {
                    self.env = self.heap.get_env(self.env).parent.unwrap();
                }
                // CheckedGlobalRef
                0x40_u8 => {
                    let idx = self.fetch_u16_usize();
                    let v = self.global_env[idx];
                    if v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.val = v;
                    }
                }
                // GlobalRef
                0x41_u8 => {
                    let idx = self.fetch_u16_usize();
                    self.val = self.global_env[idx];
                }
                // PushCheckedGlobalRef
                0x42_u8 => {
                    let idx = self.fetch_u16_usize();
                    let v = self.global_env[idx];
                    if v == Datum::Undefined {
                        panic!("Access to undefined variable");
                    } else {
                        self.stack.push(v);
                    }
                }
                // CheckedGlobalRef
                0x43_u8 => {
                    let idx = self.fetch_u16_usize();
                    self.stack.push(self.global_env[idx])
                }
                // GlobalSet
                0x44_u8 => {
                    let idx = self.fetch_u16_usize();
                    self.global_env[idx] = self.val;
                }
                // Call1
                0x50_u8 => {
                    let idx = self.fetch_u16_usize();
                    let arg0 = self.val;
                    self.val = self.call_1(idx, arg0).unwrap();
                }
                // Call2
                0x51_u8 => {
                    let idx = self.fetch_u16_usize();
                    let arg0 = self.val;
                    let arg1 = self.arg1;
                    self.val = self.call_2(idx, arg0, arg1).unwrap();
                }
                // Call3
                0x52_u8 => {
                    let idx = self.fetch_u16_usize();
                    let arg0 = self.val;
                    let arg1 = self.arg1;
                    let arg2 = self.arg2;
                    self.val = self.call_3(idx, arg0, arg1, arg2).unwrap();
                }
                // CallN
                0x53_u8 => {
                    let idx = self.fetch_u16_usize();
                    let given = self.fetch_u8() as usize;
                    let at = self.stack.len() - given;
                    let mut args = self.stack.split_off(at);
                    self.val = self.call_n(idx, &mut args).unwrap();
                }
                // ShallowArgumentRef
                0x60_u8 => {
                    let j = self.fetch_u16_usize();
                    let env = self.heap.get_env(self.env);
                    self.val = env.shallow_ref(j);
                }
                // PushShallowArgumentRef
                0x61_u8 => {
                    let j = self.fetch_u16_usize();
                    let env = self.heap.get_env(self.env);
                    self.stack.push(env.shallow_ref(j));
                }
                // ShallowArgumentSet
                0x62_u8 => {
                    let j = self.fetch_u16_usize();
                    let env = self.heap.get_env_mut(self.env);
                    env.shallow_set(j, self.val);
                }
                // DeepArgumentRef
                0x63_u8 => {
                    let i = self.fetch_u16_usize();
                    let j = self.fetch_u16_usize();
                    self.val = self.heap.env_deep_ref(self.env, i, j);
                }
                // PushDeepArgumentRef
                0x64_u8 => {
                    let i = self.fetch_u16_usize();
                    let j = self.fetch_u16_usize();
                    self.stack.push(self.heap.env_deep_ref(self.env, i, j));
                }
                // DeepArgumentSet
                0x65_u8 => {
                    let i = self.fetch_u16_usize();
                    let j = self.fetch_u16_usize();
                    self.heap.env_deep_set(self.env, i, j, self.val);
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
                    if self.val.is_equal(&Datum::Integer(0), &self.heap).unwrap() {
                        self.seek_current(offset);
                    }
                }
                // JumpNotZero
                0x76_u8 => {
                    let offset = self.fetch_u32();
                    if !self.val.is_equal(&Datum::Integer(0), &self.heap).unwrap() {
                        self.seek_current(offset);
                    }
                }

                // FixClosure
                0x80_u8 => {
                    let arity = self.fetch_u16_usize();

                    // The next instruction, a jump behind the closure,
                    // needs to be scipped (1 byte type + 4 bytes addr)
                    let closure = Datum::Closure(self.pc + 5, arity, false, self.env);
                    self.val = closure;
                }
                // DottedClosure
                0x81_u8 => {
                    let arity = self.fetch_u16_usize();

                    let closure = Datum::Closure(self.pc + 5, arity, true, self.env);
                    self.val = closure;
                }
                // StoreArgument
                0x82_u8 => unimplemented!(),
                // ConsArgument
                0x83_u8 => unimplemented!(),
                // AllocateFrame
                0x84_u8 => unimplemented!(),
                // AllocateFillFrame
                0x85_u8 => {
                    let size = self.fetch_u8_usize();
                    let mut frame = Vec::with_capacity(size);
                    for _ in 0..size {
                        let v = self.checked_pop()?;
                        frame.push(v);
                    }
                    self.val = self.heap.make_activation_frame(frame);
                }
                // AllocateDottedFrame
                //
                // The same as `AllocateFrame`,
                // just sets the last element to '()
                // so that `ConsArgument` can add the dotted arguments to it
                0x86_u8 => unimplemented!(),
                // FunctionInvoke
                // TODO: Include function symbol for debugging
                v @ 0x87_u8 | v @ 0x88_u8 => {
                    let is_tail = v == 0x88_u8;
                    if !is_tail { self.preserve_env(); }

                    // Must be constructed first,
                    // because the arguments are on top of the function
                    // on the stack
                    let size = self.fetch_u8_usize();
                    let mut elems = Vec::with_capacity(size);
                    for _ in 0..size {
                        elems.push(self.checked_pop()?);
                    }

                    // The function is the topmost element on the stack
                    match self.checked_pop()? {
                        Datum::Closure(offset, arity, dotted, env) => {
                            if !is_tail { self.pc_stack.push(self.pc); }
                            let mut new_env = Env::new(Some(env));
                            if dotted {
                                if (size + 1) < arity {
                                    panic!("Incorrect arity");
                                }

                                let rest = elems.split_off(arity - 1);
                                elems.push(self.heap.make_list_from_vec(rest));
                            } else if arity != size {
                                panic!("Incorrect arity, expected {}, got {}", arity, size);
                            }
                            new_env.extend(elems);
                            self.env = self.heap.insert_env(new_env);
                            self.pc = offset;
                        }
                        Datum::Builtin(ref typ, idx, ref arity) => {
                            let idx = idx as usize;
                            arity.check(size);

                            match typ {
                                LispFnType::Variadic => {
                                    let res = self.call_n(idx, &mut elems);
                                    self.val = res.unwrap();
                                }
                                LispFnType::Fixed1 => {
                                    let arg1 = elems[0];
                                    let res = self.call_1(idx, arg1);
                                    self.val = res.unwrap();
                                }
                                LispFnType::Fixed2 => {
                                    let arg1 = elems[0];
                                    let arg2 = elems[1];
                                    let res = self.call_2(idx, arg1, arg2);
                                    self.val = res.unwrap();
                                }
                                LispFnType::Fixed3 => {
                                    let arg1 = elems[0];
                                    let arg2 = elems[1];
                                    let arg3 = elems[2];
                                    let res = self.call_3(idx, arg1, arg2, arg3);
                                    self.val = res.unwrap();
                                }
                            }
                        }
                        other => panic!("Trying to invoke non function {:?}", other)
                    }
                }
                _ => unimplemented!(),
            }
            if self.heap.should_gc() {
                self.heap_gc();
            }
        }

        Ok(())
    }

    fn heap_gc(&mut self) {
        // TODO: Make sure to reset these after they are used
        self.heap.root_datum(self.val);
        self.heap.root_datum(self.arg1);
        self.heap.root_datum(self.arg2);
        self.heap.root_datum(self.fun);
        for d in &self.stack {
            self.heap.root_datum(*d);
        }

        // TODO: Find a better way to do this,
        // something like "hard-rooted" datums
        for d in &self.constants {
            self.heap.root_datum(*d);
        }
        for d in &self.global_env {
            self.heap.root_datum(*d);
        }

        self.heap.root_env(self.env);
        for env in &self.env_stack {
            self.heap.root_env(*env);
        }

        self.heap.mark();
        self.heap.sweep();
    }
}
