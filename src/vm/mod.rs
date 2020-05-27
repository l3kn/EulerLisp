//! Virtual Machine

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fs;
use std::fs::File;
use std::io::{self, Read, Write};
use std::rc::Rc;

use crate::builtin::{self, BuiltinRegistry};
use crate::compiler::{Compiler, Program};
use crate::env::{Env, EnvRef};
use crate::instruction::Instruction;
use crate::parser::Parser;
use crate::symbol_table::Symbol;
use crate::value::{LispAdd, LispDiv, LispIntegerDiv, LispMul, LispRem, LispSub};
use crate::{Arity, LispFn1, LispFn2, LispFn3, LispFnN};
use crate::{LispResult, Value};

mod bytecode;
mod context;
mod error;

use bytecode::Bytecode;
pub use context::Context;
pub use error::Error;

pub struct VM {
    pub val: Value,
    pub arg1: Value,
    pub arg2: Value,
    fun: Value,
    env: EnvRef,
    stack: Vec<Value>,
    env_stack: Vec<EnvRef>,
    pub bytecode: Bytecode,
    pub output: Rc<RefCell<Write>>,
    pub context: Rc<Context>,
    compiler: Compiler,
    parser: Parser,
}

impl VM {
    pub fn new() -> Self {
        Self::with_output(Rc::new(RefCell::new(io::stdout())))
    }

    pub fn with_output(output: Rc<RefCell<Write>>) -> Self {
        let stack = Vec::with_capacity(1000);
        let local_env = Env::new(vec![], None);
        let context = Rc::new(Context::new());

        // Start with one "Finish" instruction,
        // the pc pointing behind it and an empty pc stack.
        // This way the last return (e.g. from a tail optimized function)
        // ends the execution.
        let mut vm = VM {
            bytecode: Bytecode::new(vec![0x01_u8], 1),
            output: output,
            val: Value::Undefined,
            arg1: Value::Undefined,
            arg2: Value::Undefined,
            fun: Value::Undefined,
            env: Rc::new(local_env),
            stack,
            env_stack: Vec::new(),
            compiler: Compiler::new(context.clone()),
            context,
            parser: Parser::new(),
        };

        builtin::load(&mut vm);

        vm
    }

    pub fn load_file(&mut self, path: &str, tail: bool) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input)
            .expect("Could not read file");

        self.load_str(&input[..], tail, Some(path.to_string()));
    }

    pub fn load_str(
        &mut self,
        input: &str,
        tail: bool,
        source: Option<String>,
    ) -> LispResult<Value> {
        self.parser.load_string(input.to_string(), source);

        // TODO: convert parser errors to lisp errors
        let mut datums: Vec<Value> = Vec::new();
        while let Some(next) = self.parser.next_value().expect("Failed to parse") {
            datums.push(next)
        }

        for datum in datums {
            let program = self.compiler.compile(vec![datum], tail)?;
            self.append_program(program);
            self.run();
        }

        Ok(self.val.clone())
    }

    pub fn load_stdlib(&mut self) {
        // TODO: Better handling of stdlib
        let paths = fs::read_dir("/home/leon/src/euler_lisp/stdlib").unwrap();
        let mut string_paths: Vec<String> = paths
            .map(|p| p.unwrap().path().display().to_string())
            .collect();
        string_paths.sort();
        for path in string_paths {
            // println!("Loading file {}", path);
            self.load_file(&path, false);
        }
        // self.vm.run();
        // if let Err(err) = self.run() {
        //     println!("Error: {}", err);
        // }
    }

    // TODO: Find a way to remove this indirection
    pub fn set_pc(&mut self, v: usize) {
        self.bytecode.set_pc(v);
    }

    pub fn append_program(&mut self, program: Program) {
        let Program { instructions } = program;
        self.bytecode.extend(instructions);
    }

    fn checked_pop(&mut self) -> LispResult<Value> {
        if let Some(dat) = self.stack.pop() {
            Ok(dat)
        } else {
            Err(Error::StackUnderflow(self.bytecode.pc))?
        }
    }

    fn preserve_env(&mut self) {
        self.env_stack.push(self.env.clone());
    }

    pub fn run(&mut self) -> LispResult<()> {
        // let end = self.bytecode.len();
        // while self.bytecode.pc != end {
        while self.bytecode.pc != self.bytecode.len() {
            let inst = self.bytecode.fetch_u8();
            self.run_instruction(inst)?;
        }

        Ok(())
    }

    fn run_instruction(&mut self, inst: u8) -> LispResult<()> {
        match inst {
            // Return
            0x00_u8 => self.bytecode.restore_pc()?,
            // Finish
            // 0x01_u8 => break,
            // FIXME: I don't like this change,
            // but in need to break out of the `run()` loop.
            // Returning extra information with the result
            // could be a better solution.
            0x01_u8 => self.bytecode.pc = self.bytecode.len(),
            // Inc
            0x10_u8 => {
                self.val = match &self.val {
                    Value::Integer(x) => Value::Integer(x + 1),
                    Value::Float(x) => Value::Float(x + 1.0),
                    Value::Rational(x) => Value::Rational(x + 1),
                    Value::Bignum(x) => Value::Bignum(x + 1),
                    other => panic!("INC not implemented for {}", other),
                }
            }
            // Dec
            0x11_u8 => {
                self.val = match &self.val {
                    Value::Integer(x) => Value::Integer(x - 1),
                    Value::Float(x) => Value::Float(x - 1.0),
                    Value::Rational(x) => Value::Rational(x - 1),
                    Value::Bignum(x) => Value::Bignum(x - 1),
                    other => panic!("DEC not implemented for {}", other),
                }
            }
            // Add
            0x12_u8 => self.val = self.val.add(&self.arg1)?,
            // Sub
            0x13_u8 => self.val = self.val.sub(&self.arg1)?,
            // Mul
            0x14_u8 => self.val = self.val.mul(&self.arg1)?,
            // Div
            0x15_u8 => self.val = self.val.div(&self.arg1)?,
            // Mod
            0x16_u8 => self.val = self.val.rem(&self.arg1)?,
            // IntDiv
            0x17_u8 => self.val = self.val.integer_div(&self.arg1)?,
            // Not
            0x18_u8 => {
                self.val = Value::Bool(self.val == Value::Bool(false));
            }
            // Equal
            0x19_u8 => {
                self.val = Value::Bool(self.arg1.is_equal(&self.val)?);
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
                self.val = Value::Bool(self.val.compare(&self.arg1)? == Ordering::Greater);
            }
            // Gte
            0x1D_u8 => {
                self.val = Value::Bool(self.val.compare(&self.arg1)? != Ordering::Less);
            }
            // Lt
            0x1E_u8 => {
                self.val = Value::Bool(self.val.compare(&self.arg1)? == Ordering::Less);
            }
            // Lte
            0x1F_u8 => {
                self.val = Value::Bool(self.val.compare(&self.arg1)? != Ordering::Greater);
            }

            // Fst
            0x20_u8 => {
                let a = self.val.take();
                self.val = a.as_pair()?.get_fst().clone();
            }
            // Rst
            0x21_u8 => {
                let a = self.val.take();
                self.val = a.as_pair()?.get_rst().clone();
            }
            // Cons
            0x22_u8 => {
                let a = self.val.take();
                let b = self.arg1.take();
                self.val = Value::make_pair(a, b);
            }
            // IsZero
            0x23_u8 => {
                self.val = Value::Bool(self.val.is_equal(&Value::Integer(0))?);
            }
            // IsNil
            0x24_u8 => self.val = Value::Bool(self.val == Value::Nil),
            // VectorRef
            0x25_u8 => {
                let vector = self.val.take();
                let vector = vector.as_vector()?;
                let index: usize = self.arg1.take().try_into()?;

                // TODO: Convert errors
                match vector.get(index) {
                    Some(e) => self.val = e.clone(),
                    None => panic!("vector-ref index out of bounds"),
                }
            }
            // VectorSet
            0x26_u8 => {
                let mut vector = self.val.as_mut_vector()?;
                let index: usize = self.arg1.take().try_into()?;

                if index < vector.len() {
                    vector[index] = self.arg2.take();
                } else {
                    panic!("vector-set index out of bounds")
                }
            }

            // Constant
            0x30_u8 => {
                let i = self.bytecode.fetch_u16_as_usize();
                self.val = self.context.get_constant(i);
            }
            // PushConstant
            0x31_u8 => {
                let i = self.bytecode.fetch_u16_as_usize();
                let constant = self.context.get_constant(i);
                self.stack.push(constant);
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
                    Err(Error::EnvStackUnderflow(self.bytecode.pc))?;
                }
            }
            // ExtendEnv
            0x38_u8 => {
                if let Value::ActivationFrame(elems) = self.val.take() {
                    let new_env = Env::new(elems, Some(self.env.clone()));
                    self.env = Rc::new(new_env);
                } else {
                    panic!("ExtendEnv without a activation frame in val");
                }
            }
            // UnlinkEnv
            0x39_u8 => {
                self.env = self.env.parent.clone()?;
            }

            // CheckedGlobalRef
            0x40_u8 => {
                let index = self.bytecode.fetch_u16_as_usize();
                let v = self.context.get_global(index);
                if v == Value::Undefined {
                    panic!("Access to undefined variable");
                } else {
                    self.val = v;
                }
            }
            // GlobalRef
            0x41_u8 => {
                let index = self.bytecode.fetch_u16_as_usize();
                let v = self.context.get_global(index);
                self.val = v;
            }
            // PushCheckedGlobalRef
            0x42_u8 => {
                let index = self.bytecode.fetch_u16_as_usize();
                let v = self.context.get_global(index);
                if v == Value::Undefined {
                    panic!("Access to undefined variable");
                } else {
                    self.stack.push(v);
                }
            }
            // PushGlobalRef
            0x43_u8 => {
                let index = self.bytecode.fetch_u16_as_usize();
                let v = self.context.get_global(index);
                self.stack.push(v);
            }
            // GlobalSet
            0x44_u8 => {
                let index = self.bytecode.fetch_u16_as_usize();
                self.context.set_global(index, self.val.take());
            }
            // ShallowArgumentRef
            0x60_u8 => {
                let j = self.bytecode.fetch_u16_as_usize();
                self.val = self.env.shallow_ref(j);
            }
            // PushShallowArgumentRef
            0x61_u8 => {
                let j = self.bytecode.fetch_u16_as_usize();
                self.stack.push(self.env.shallow_ref(j));
            }
            // ShallowArgumentSet
            0x62_u8 => {
                let j = self.bytecode.fetch_u16_as_usize();
                self.env.shallow_set(j, self.val.take());
            }
            // DeepArgumentRef
            0x63_u8 => {
                let i = self.bytecode.fetch_u16_as_usize();
                let j = self.bytecode.fetch_u16_as_usize();
                self.val = self.env.deep_ref(i, j);
            }
            // PushDeepArgumentRef
            0x64_u8 => {
                let i = self.bytecode.fetch_u16_as_usize();
                let j = self.bytecode.fetch_u16_as_usize();
                self.stack.push(self.env.deep_ref(i, j));
            }
            // DeepArgumentSet
            0x65_u8 => {
                let i = self.bytecode.fetch_u16_as_usize();
                let j = self.bytecode.fetch_u16_as_usize();
                self.env.deep_set(i, j, self.val.take());
            }

            // Jump
            0x70_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                self.bytecode.inc_pc(offset);
            }
            // JumpTrue
            0x71_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                if self.val.is_true() {
                    self.bytecode.inc_pc(offset);
                }
            }
            // JumpFalse
            0x72_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                if self.val.is_false() {
                    self.bytecode.inc_pc(offset);
                }
            }
            // JumpNil
            0x73_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                if self.val == Value::Nil {
                    self.bytecode.inc_pc(offset);
                }
            }
            // JumpNotNil
            0x74_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                if self.val != Value::Nil {
                    self.bytecode.inc_pc(offset);
                }
            }
            // JumpZero
            0x75_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                if self.val.is_equal(&Value::Integer(0))? {
                    self.bytecode.inc_pc(offset);
                }
            }
            // JumpNotZero
            0x76_u8 => {
                let offset = self.bytecode.fetch_u32_as_usize();
                if !self.val.is_equal(&Value::Integer(0))? {
                    self.bytecode.inc_pc(offset);
                }
            }

            // FixClosure
            0x80_u8 => {
                let arity = self.bytecode.fetch_u16_as_usize();

                // The next instruction, a jump behind the closure,
                // needs to be scipped (1 byte type + 4 bytes addr)
                let closure = Value::Closure(self.bytecode.pc + 5, arity, false, self.env.clone());
                self.val = closure;
            }
            // DottedClosure
            0x81_u8 => {
                let arity = self.bytecode.fetch_u16_as_usize();

                let closure = Value::Closure(self.bytecode.pc + 5, arity, true, self.env.clone());
                self.val = closure;
            }
            // StoreArgument
            0x82_u8 => {
                unimplemented!();
                // let index = self.bytecode.fetch_u8_as_usize();
                // self.frame[index] = self.checked_pop()?;
            }
            // ConsArgument
            0x83_u8 => {
                unimplemented!();
                // let index = self.bytecode.fetch_u8_as_usize();
                // self.frame[index] = Value::make_pair(self.frame[index].take(), self.val.take());
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
                for _ in 0..size {
                    let v = self.checked_pop()?;
                    frame.push(v);
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
                let fun = self.checked_pop()?;
                self.invoke_function(fun, elems, is_tail)?;
            }
            // call/cc
            0x89_u8 => {
                let mut pc_stack = self.bytecode.get_pc_stack();

                // self.preserve_env();

                pc_stack.push(self.bytecode.pc);
                let continuation =
                    Value::Continuation(self.stack.clone(), self.env_stack.clone(), pc_stack);
                let fun = self.val.take();
                self.invoke_function(fun, vec![continuation], true)?;

                // if let Some(env) = self.env_stack.pop() {
                //     self.env = env;
                // } else {
                //     Err(Error::EnvStackUnderflow(self.bytecode.pc))?;
                // }
            }
            // apply
            0x90_u8 => {
                let fun = self.val.take();
                let args = self.arg1.as_list()?;
                self.invoke_function(fun, args, false);
            }
            // eval
            0x91_u8 => {
                unimplemented!();
                let input = self.val.take();
                let mut program = self.compiler.compile(vec![input], false)?;
                self.bytecode.store_pc();

                self.preserve_env();
                program.instructions.push((Instruction::RestoreEnv, None));
                program.instructions.push((Instruction::Return, None));

                let start = self.bytecode.len();
                self.append_program(program);
                self.set_pc(start as usize);
            }
            // integer
            0x92_u8 => {
                let i = self.bytecode.fetch_u16() as isize;
                self.val = Value::Integer(i);
            }
            // push integer
            0x93_u8 => {
                let i = self.bytecode.fetch_u16() as isize;
                self.stack.push(Value::Integer(i));
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    // pub fn eval(&mut self, arg: Value) -> LispResult<Value> {}

    pub fn invoke_function(
        &mut self,
        fun: Value,
        mut args: Vec<Value>,
        is_tail: bool,
    ) -> LispResult<()> {
        match fun {
            Value::Continuation(stack, env_stack, pc_stack) => {
                // Continuations don't care about whether they run in tail position or not,
                // as the pc stored on the pc_stack would be overwritten
                self.stack = stack;
                self.env_stack = env_stack;
                self.bytecode.set_pc_stack(pc_stack);
                self.bytecode.restore_pc();
                self.val = args[0].take();
            }
            Value::Closure(offset, arity, dotted, ref env) => {
                if !is_tail {
                    self.bytecode.store_pc();
                }
                if dotted {
                    if (args.len() + 1) < arity {
                        panic!("Incorrect arity");
                    }

                    let rest = args.split_off(arity - 1);
                    args.push(Value::make_list_from_vec(rest));
                } else if arity != args.len() {
                    panic!("Incorrect arity, expected {}, got {}", arity, args.len());
                }
                let new_env = Env::new(args, Some(env.clone()));
                self.env = Rc::new(new_env);
                self.bytecode.set_pc(offset);
            }
            Value::Builtin1(_, ref fun) => {
                let arg1 = args[0].take();
                self.val = fun(arg1, &self)?;
            }
            Value::Builtin2(_, ref fun) => {
                let arg1 = args[0].take();
                let arg2 = args[1].take();
                self.val = fun(arg1, arg2, &self)?;
            }
            Value::Builtin3(_, ref fun) => {
                let arg1 = args[0].take();
                let arg2 = args[1].take();
                let arg3 = args[2].take();
                self.val = fun(arg1, arg2, arg3, &self)?;
            }
            Value::BuiltinN(_, ref fun, ref arity) => {
                arity.check(args.len());
                self.val = fun(&mut args, &self)?;
            }
            // TODO: Error type
            other => panic!("Trying to invoke non function {}", other),
        }
        Ok(())
    }
}

impl BuiltinRegistry for VM {
    fn register1(&mut self, name: &str, fun: LispFn1) {
        let key = Symbol::intern(name);
        self.context.add_global(key, Value::Builtin1(key, fun));
    }

    fn register2(&mut self, name: &str, fun: LispFn2) {
        let key = Symbol::intern(name);
        self.context.add_global(key, Value::Builtin2(key, fun));
    }

    fn register3(&mut self, name: &str, fun: LispFn3) {
        let key = Symbol::intern(name);
        self.context.add_global(key, Value::Builtin3(key, fun));
    }

    fn register_var(&mut self, name: &str, fun: LispFnN, arity: Arity) {
        let key = Symbol::intern(name);
        self.context
            .add_global(key, Value::BuiltinN(key, fun, arity));
    }
}
