use std::cell::RefCell;
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
use std::rc::Rc;

use crate::builtin::{self, BuiltinRegistry};
use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::symbol_table::Symbol;
use crate::vm::VM;
use crate::{LispResult, Value};

/// Wrapper around a compiler and VM
/// to allow easy execution of programs
pub struct Evaluator {
    compiler: Compiler,
    vm: VM,
    parser: Parser,
}

impl Evaluator {
    pub fn new(output: Rc<RefCell<Write>>, stdlib: bool) -> Self {
        let mut registry = BuiltinRegistry::new();
        builtin::load(&mut registry);

        let mut vm = VM::new(output);
        let mut compiler = Compiler::new();

        // Register builtin functions in the compiler and the VM
        for (string_key, fun) in &registry.fns_1 {
            let key = Symbol::intern(string_key);
            compiler.bind_global(key);
            vm.add_global(Value::Builtin1(key, fun.clone()));
        }
        for (string_key, fun) in &registry.fns_2 {
            let key = Symbol::intern(string_key);
            compiler.bind_global(key);
            vm.add_global(Value::Builtin2(key, fun.clone()));
        }
        for (string_key, fun) in &registry.fns_3 {
            let key = Symbol::intern(string_key);
            compiler.bind_global(key);
            vm.add_global(Value::Builtin3(key, fun.clone()));
        }
        for (string_key, fun, arity) in &registry.fns_n {
            let key = Symbol::intern(string_key);
            compiler.bind_global(key);
            vm.add_global(Value::BuiltinN(key, fun.clone(), *arity));
        }

        let mut eval = Evaluator {
            compiler,
            vm,
            parser: Parser::new(),
        };

        if stdlib {
            eval.load_stdlib();
        }

        eval
    }

    fn load_stdlib(&mut self) {
        // TODO: Is there a more elegant way to do this?
        let paths = fs::read_dir("./stdlib").unwrap();
        let mut string_paths: Vec<String> = paths
            .map(|p| p.unwrap().path().display().to_string())
            .collect();
        string_paths.sort();
        for path in string_paths {
            // println!("Loading file {}", path);
            self.load_file(&path, false);
        }
        if let Err(err) = self.vm.run() {
            println!("Error: {}", err);
        }
    }

    pub fn load_file(&mut self, path: &str, tail: bool) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input)
            .expect("Could not read file");

        self.load_str(&input[..], tail, Some(path.to_string()));
    }

    fn load_str(&mut self, input: &str, tail: bool, source: Option<String>) {
        self.parser.load_string(input.to_string(), source);

        // TODO: convert parser errors to lisp errors
        let mut datums: Vec<Value> = Vec::new();
        while let Some(next) = self.parser.next_value().expect("Failed to parse") {
            datums.push(next)
        }

        match self.compiler.compile(datums, tail) {
            Ok(program) => self.vm.append_program(program),
            Err(err) => println!("{}", err),
        }
    }

    pub fn eval_str(&mut self, input: &str) -> LispResult<Value> {
        // To make the REPL work, jump to the end of the old program
        // every time new code is evaluated
        let start = self.vm.bytecode.len();
        self.load_str(input, false, None);

        // TODO: Find a more elegant way to do this.
        // The problem occurs when `input` is e.g. (defcons ...),
        // so that no new instructions are added
        if start == self.vm.bytecode.len() {
            Ok(Value::Undefined)
        } else {
            self.vm.set_pc(start as usize);
            self.run();
            Ok(self.vm.val.take())
        }
    }

    pub fn run(&mut self) {
        if let Err(err) = self.vm.run() {
            println!("Err: {}", err);
        }
    }
}
