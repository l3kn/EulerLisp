use std::fs;
use std::fs::File;
use std::io::Read;

use crate::{Datum, Expression, LispFnType};
use crate::builtin::{self, BuiltinRegistry};
use crate::compiler::{Compiler, Program};
use crate::instruction::{Instruction, LabeledInstruction};
use crate::parser::Parser;
use crate::heap::Heap;

/// Compile a program together with the stdlib
/// and then output its instructions
/// (not reduced to their byte format)
/// in a human readable form
pub struct Debugger {
    compiler: Compiler,
    constants: Vec<Datum>,
    builtins: BuiltinRegistry,
    heap: Heap
}

impl Debugger {
    pub fn new(stdlib: bool) -> Self {
        let mut registry = BuiltinRegistry::new();
        builtin::load(&mut registry);

        let mut eval = Debugger {
            compiler: Compiler::new(registry.clone()),
            constants: Vec::new(),
            builtins: registry,
            heap: Heap::new(),
        };

        if stdlib {
            eval.load_stdlib();
        }

        eval
    }

    fn load_stdlib(&mut self) {
        let mut full = String::new();

        // TODO: Is there a more elegant way to do this?
        let paths = fs::read_dir("./stdlib").unwrap();
        let mut string_paths: Vec<String> = paths
            .map(|p| p.unwrap().path().display().to_string())
            .collect();
        string_paths.sort();
        for path in string_paths {
            let mut f = File::open(path).expect("Could not open file");
            let mut input = String::new();
            f.read_to_string(&mut input).expect("Could not read file");
            full += &input;
        }

        // TODO: Expose real source here, just like in `Evaluator`
        let mut parser = Parser::from_string(&full, None);

        // TODO: convert parser errors to lisp errors
        let mut datums: Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        // Ingnore the results,
        // just make sure the compiler contains all the macros & globals
        // from the stdlib
        let Program { constants, .. } = self.compiler.compile(datums, true);
        let constants: Vec<Datum> =
            constants.into_iter().map(|c| c.to_datum(
                &mut self.compiler.symbol_table,
                &mut self.heap
            )).collect();
        self.constants.extend(constants);
    }

    pub fn debug_file(&mut self, path: &str) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input)
            .expect("Could not read file");

        let mut parser = Parser::from_string(&input, Some(path.to_string()));

        // TODO: convert parser errors to lisp errors
        let mut datums: Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        let Program {
            constants,
            num_globals,
            instructions,
        } = self.compiler.compile(datums, true);
        let constants: Vec<Datum> =
            constants.into_iter().map(|c| c.to_datum(
                &mut self.compiler.symbol_table,
                &mut self.heap
            )).collect();
        self.constants.extend(constants);

        println!("New Globals: {}", num_globals);
        println!("Instructions:");

        for inst in &instructions {
            self.prettyprint(inst);
        }
    }

    pub fn prettyprint(&self, linst: &LabeledInstruction) {
        let st = &self.compiler.symbol_table;
        let (inst, label) = linst;
        print!("   ");
        match *inst {
            Instruction::Finish => println!("FINISH"),
            Instruction::Inc => println!("INC"),
            Instruction::Dec => println!("DEC"),
            Instruction::Add => println!("ADD"),
            Instruction::Sub => println!("SUB"),
            Instruction::Mul => println!("MUL"),
            Instruction::Div => println!("DIV"),
            Instruction::Mod => println!("MOD"),
            Instruction::IntDiv => println!("IDIV"),
            Instruction::Fst => println!("FST"),
            Instruction::Rst => println!("RST"),
            Instruction::Cons => println!("CONS"),
            Instruction::Not => println!("NOT"),
            Instruction::Eq => println!("EQ"),
            Instruction::Neq => println!("NEQ"),
            Instruction::Equal => println!("EQUAL"),
            Instruction::Lt => println!("LT"),
            Instruction::Gt => println!("GT"),
            Instruction::Lte => println!("LTE"),
            Instruction::Gte => println!("GTE"),
            Instruction::IsZero => println!("ZERO?"),
            Instruction::IsNil => println!("NIL?"),
            Instruction::VectorRef => println!("VECTOR-REF"),
            Instruction::VectorSet => println!("VECTOR-SET!"),
            Instruction::Constant(i) => {
                println!("CONSTANT ${}", self.constants[i as usize].to_string(&st, &self.heap, true));
            }
            Instruction::PushConstant(i) => {
                println!("CONSTANT ${}", self.constants[i as usize].to_string(&st, &self.heap, true));
            }
            Instruction::PushValue => println!("PUSH-VALUE"),
            Instruction::GlobalSet(i) => println!("GLOBAL-SET {}", i),
            Instruction::GlobalRef(i) => println!("GLOBAL-REF {}", i),
            Instruction::PushGlobalRef(i) => println!("PUSH-GLOBAL-REF {}", i),
            Instruction::CheckedGlobalRef(i) => println!("CHECKED-GLOBAL-REF {}", i),
            Instruction::PushCheckedGlobalRef(i) => println!("PUSH-CHECKED-GLOBAL-REF {}", i),
            Instruction::ShallowArgumentRef(i) => println!("SHALLOW-ARGUMENT-REF {}", i),
            Instruction::PushShallowArgumentRef(i) => println!("PUSH-SHALLOW-ARGUMENT-REF {}", i),
            Instruction::ShallowArgumentSet(i) => println!("SHALLOW-ARGUMENT-SET {}", i),
            Instruction::DeepArgumentRef(i, j) => println!("DEEP-ARGUMENT-REF {} {}", i, j),
            Instruction::PushDeepArgumentRef(i, j) => {
                println!("PUSH-DEEP-ARGUMENT-REF {} {}", i, j)
            }
            Instruction::DeepArgumentSet(i, j) => println!("DEEP-ARGUMENT-SET {} {}", i, j),
            Instruction::PreserveEnv => println!("PRESERVE-ENV"),
            Instruction::RestoreEnv => println!("RESTORE-ENV"),
            Instruction::ExtendEnv => println!("EXTEND-ENV"),
            Instruction::UnlinkEnv => println!("UNLINK-ENV"),
            Instruction::Call1(id) => {
                let name = self.builtins.lookup_name(LispFnType::Fixed1, id);
                println!("CALL1 {}", name);
            }
            Instruction::Call2(id) => {
                let name = self.builtins.lookup_name(LispFnType::Fixed2, id);
                println!("CALL2 {}", name);
            }
            Instruction::Call3(id) => {
                let name = self.builtins.lookup_name(LispFnType::Fixed3, id);
                println!("CALL3 {}", name);
            }
            Instruction::CallN(id, arity) => {
                let name = self.builtins.lookup_name(LispFnType::Variadic, id);
                println!("CALLN {} (arity {})", name, arity);
            }
            Instruction::Jump(offset) => println!("JUMP @{}", offset),
            Instruction::JumpFalse(offset) => println!("JUMP-FALSE @{}", offset),
            Instruction::JumpTrue(offset) => println!("JUMP-TRUE @{}", offset),
            Instruction::JumpNil(offset) => println!("JUMP-NIL @{}", offset),
            Instruction::JumpNotNil(offset) => println!("JUMP-NOT-NIL @{}", offset),
            Instruction::JumpZero(offset) => println!("JUMP-ZERO @{}", offset),
            Instruction::JumpNotZero(offset) => println!("JUMP-NOT-ZERO @{}", offset),
            Instruction::FixClosure(arity) => println!("CREATE-CLOSURE {}", arity),
            Instruction::DottedClosure(arity) => println!("CREATE-CLOSURE {}", arity),
            Instruction::Return => println!("RETURN"),
            Instruction::StoreArgument(idx) => println!("STORE-ARGUMENT {}", idx),
            Instruction::ConsArgument(idx) => println!("CONS-ARGUMENT {}", idx),
            Instruction::AllocateFrame(idx) => println!("ALLOCATE-FRAME {}", idx),
            Instruction::AllocateFillFrame(idx) => println!("ALLOCATE-FILL-FRAME {}", idx),
            Instruction::AllocateDottedFrame(idx) => println!("ALLOCATE-DOTTED-FRAME {}", idx),
            Instruction::PopFunction => println!("POP-FUNCTION"),
            Instruction::PopArg1 => println!("POP-ARG1"),
            Instruction::PopArg2 => println!("POP-ARG2"),
            Instruction::FunctionInvoke(tail, arity) => println!("FUNCTION-INVOKE tail: {}, arity: {}", tail, arity),
        }

        if let Some(l) = label {
            println!("{}:", l);
        }
    }
}
