use std::fs;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use crate::builtin::{self, BuiltinRegistry};
use crate::compiler::{Compiler, Program};
use crate::instruction::{Instruction, LabeledInstruction};
use crate::parser::Parser;
use crate::symbol_table::Symbol;
use crate::vm::Context;
use crate::Value;
use crate::{Arity, LispFn1, LispFn2, LispFn3, LispFnN};

/// Compile a program together with the stdlib
/// and then output its instructions
/// (not reduced to their byte format)
/// in a human readable form
pub struct Debugger {
    compiler: Compiler,
    constants: Vec<Value>,
    globals: Vec<String>,
    parser: Parser,
}

// impl Debugger {
//     pub fn new(stdlib: bool) -> Self {
//         let context = Rc::new(Context::new());
//         let mut debugger = Debugger {
//             compiler: Compiler::new(context),
//             constants: Vec::new(),
//             parser: Parser::new(),
//             globals: vec![],
//         };

//         builtin::load(&mut debugger);

//         if stdlib {
//             debugger.load_stdlib();
//         }

//         debugger
//     }

//     fn load_stdlib(&mut self) {
//         // TODO: Is there a more elegant way to do this?
//         let paths = fs::read_dir("./stdlib").unwrap();
//         let mut string_paths: Vec<String> = paths
//             .map(|p| p.unwrap().path().display().to_string())
//             .collect();
//         string_paths.sort();
//         for path in string_paths {
//             // println!("Loading file {}", path);
//             self.load_file(&path, false);
//         }
//     }

//     pub fn load_file(&mut self, path: &str, tail: bool) {
//         // TODO: Add IOError type
//         let mut file = File::open(path).expect("Could not open file");
//         let mut input = String::new();
//         file.read_to_string(&mut input)
//             .expect("Could not read file");

//         self.load_str(&input[..], tail, Some(path.to_string()));
//     }

//     fn lookup_global(&self, index: u16) -> String {
//         if let Some(g) = self.globals.get(index as usize) {
//             g.clone()
//         } else {
//             format!("g{}", index)
//         }
//     }

//     fn load_str(&mut self, input: &str, tail: bool, source: Option<String>) {
//         self.parser.load_string(input.to_string(), source);

//         // TODO: convert parser errors to lisp errors
//         let mut datums: Vec<Value> = Vec::new();
//         while let Some(next) = self.parser.next_value().expect("Failed to parse") {
//             datums.push(next)
//         }

//         self.compiler.compile(datums, tail).unwrap();
//     }

//     pub fn debug_file(&mut self, path: &str) {
//         // TODO: Add IOError type
//         let mut file = File::open(path).expect("Could not open file");
//         let mut input = String::new();
//         file.read_to_string(&mut input)
//             .expect("Could not read file");

//         self.parser.load_string(input, Some(path.to_string()));

//         // TODO: convert parser errors to lisp errors
//         let mut datums: Vec<Value> = Vec::new();
//         while let Some(next) = self.parser.next_value().expect("Failed to parse") {
//             datums.push(next)
//         }

//         let Program { instructions } = self.compiler.compile(datums, true).unwrap();

//         println!("Instructions:");

//         for inst in &instructions {
//             self.prettyprint(inst);
//         }
//     }

//     pub fn prettyprint(&self, linst: &LabeledInstruction) {
//         let (inst, label) = linst;
//         print!("   ");
//         match *inst {
//             Instruction::Finish => println!("FINISH"),
//             Instruction::Inc => println!("INC"),
//             Instruction::Dec => println!("DEC"),
//             Instruction::Add => println!("ADD"),
//             Instruction::Sub => println!("SUB"),
//             Instruction::Mul => println!("MUL"),
//             Instruction::Div => println!("DIV"),
//             Instruction::Mod => println!("MOD"),
//             Instruction::IntDiv => println!("IDIV"),
//             Instruction::Fst => println!("FST"),
//             Instruction::Rst => println!("RST"),
//             Instruction::Cons => println!("CONS"),
//             Instruction::Not => println!("NOT"),
//             Instruction::Eq => println!("EQ"),
//             Instruction::Neq => println!("NEQ"),
//             Instruction::Equal => println!("EQUAL"),
//             Instruction::Lt => println!("LT"),
//             Instruction::Gt => println!("GT"),
//             Instruction::Lte => println!("LTE"),
//             Instruction::Gte => println!("GTE"),
//             Instruction::IsZero => println!("ZERO?"),
//             Instruction::IsNil => println!("NIL?"),
//             Instruction::VectorRef => println!("VECTOR-REF"),
//             Instruction::VectorSet => println!("VECTOR-SET!"),
//             Instruction::Constant(i) => {
//                 println!("CONSTANT ${}", self.constants[i as usize]);
//             }
//             Instruction::PushConstant(i) => {
//                 println!("PUSH-CONSTANT ${}", self.constants[i as usize]);
//             }
//             Instruction::PushValue => println!("PUSH-VALUE"),
//             Instruction::GlobalSet(i) => println!("GLOBAL-SET {}", self.lookup_global(i)),
//             Instruction::GlobalRef(i) => println!("GLOBAL-REF {}", self.lookup_global(i)),
//             Instruction::PushGlobalRef(i) => println!("PUSH-GLOBAL-REF {}", self.lookup_global(i)),
//             Instruction::CheckedGlobalRef(i) => {
//                 println!("CHECKED-GLOBAL-REF {}", self.lookup_global(i))
//             }
//             Instruction::PushCheckedGlobalRef(i) => {
//                 println!("PUSH-CHECKED-GLOBAL-REF {}", self.lookup_global(i))
//             }
//             Instruction::ShallowArgumentRef(i) => println!("SHALLOW-ARGUMENT-REF {}", i),
//             Instruction::PushShallowArgumentRef(i) => println!("PUSH-SHALLOW-ARGUMENT-REF {}", i),
//             Instruction::ShallowArgumentSet(i) => println!("SHALLOW-ARGUMENT-SET {}", i),
//             Instruction::DeepArgumentRef(i, j) => println!("DEEP-ARGUMENT-REF {} {}", i, j),
//             Instruction::PushDeepArgumentRef(i, j) => {
//                 println!("PUSH-DEEP-ARGUMENT-REF {} {}", i, j)
//             }
//             Instruction::DeepArgumentSet(i, j) => println!("DEEP-ARGUMENT-SET {} {}", i, j),
//             Instruction::PreserveEnv => println!("PRESERVE-ENV"),
//             Instruction::RestoreEnv => println!("RESTORE-ENV"),
//             Instruction::ExtendEnv => println!("EXTEND-ENV"),
//             Instruction::UnlinkEnv => println!("UNLINK-ENV"),
//             Instruction::Jump(offset) => println!("JUMP @{}", offset),
//             Instruction::JumpFalse(offset) => println!("JUMP-FALSE @{}", offset),
//             Instruction::JumpTrue(offset) => println!("JUMP-TRUE @{}", offset),
//             Instruction::JumpNil(offset) => println!("JUMP-NIL @{}", offset),
//             Instruction::JumpNotNil(offset) => println!("JUMP-NOT-NIL @{}", offset),
//             Instruction::JumpZero(offset) => println!("JUMP-ZERO @{}", offset),
//             Instruction::JumpNotZero(offset) => println!("JUMP-NOT-ZERO @{}", offset),
//             Instruction::FixClosure(arity) => println!("CREATE-CLOSURE {}", arity),
//             Instruction::DottedClosure(arity) => println!("CREATE-CLOSURE {}", arity),
//             Instruction::Return => println!("RETURN"),
//             Instruction::StoreArgument(idx) => println!("STORE-ARGUMENT {}", idx),
//             Instruction::ConsArgument(idx) => println!("CONS-ARGUMENT {}", idx),
//             Instruction::AllocateFrame(idx) => println!("ALLOCATE-FRAME {}", idx),
//             Instruction::AllocateFillFrame(idx) => println!("ALLOCATE-FILL-FRAME {}", idx),
//             Instruction::AllocateDottedFrame(idx) => println!("ALLOCATE-DOTTED-FRAME {}", idx),
//             Instruction::PopFunction => println!("POP-FUNCTION"),
//             Instruction::PopArg1 => println!("POP-ARG1"),
//             Instruction::PopArg2 => println!("POP-ARG2"),
//             Instruction::FunctionInvoke(tail, arity) => {
//                 println!("FUNCTION-INVOKE tail: {}, arity: {}", tail, arity)
//             }
//             Instruction::CallCC => println!("CALL-CC"),
//             Instruction::Apply => println!("APPLY"),
//             Instruction::Eval => println!("EVAL"),
//         }

//         if let Some(l) = label {
//             println!("{}:", l);
//         }
//     }
// }

// impl BuiltinRegistry for Debugger {
//     fn register1(&mut self, name: &str, fun: LispFn1) {
//         let key = Symbol::intern(name);
//         self.compiler.bind_global(key);
//         self.globals.push(name.to_string());
//     }

//     fn register2(&mut self, name: &str, fun: LispFn2) {
//         let key = Symbol::intern(name);
//         self.compiler.bind_global(key);
//         self.globals.push(name.to_string());
//     }

//     fn register3(&mut self, name: &str, fun: LispFn3) {
//         let key = Symbol::intern(name);
//         self.compiler.bind_global(key);
//         self.globals.push(name.to_string());
//     }

//     fn register_var(&mut self, name: &str, fun: LispFnN, arity: Arity) {
//         let key = Symbol::intern(name);
//         self.compiler.bind_global(key);
//         self.globals.push(name.to_string());
//     }
// }
