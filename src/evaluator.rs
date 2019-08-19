use std::cell::RefCell;
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
use std::rc::Rc;

use crate::builtin::{self, BuiltinRegistry};
use crate::compiler::{Compiler, Program};
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;
use crate::vm::VM;
use crate::{LispErr, Symbol, Value};

/// Wrapper around a compiler and VM
/// to allow easy execution of programs
pub struct Evaluator {
    compiler: Compiler,
    vm: VM,
    // lexer: Lexer,
    parser: Parser,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Evaluator {
    pub fn new(output: Rc<RefCell<Write>>, stdlib: bool) -> Self {
        let mut symbol_table = SymbolTable::default();
        symbol_table.seed();
        let st_ref = Rc::new(RefCell::new(symbol_table));

        let mut registry = BuiltinRegistry::new(st_ref.clone());
        builtin::load(&mut registry);

        let vm = VM::new(output, st_ref.clone(), registry.clone());

        let mut eval = Evaluator {
            compiler: Compiler::new(registry),
            vm,
            parser: Parser::new(st_ref.clone()),
            symbol_table: st_ref,
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
            self.load_file(&path, false);
        }
        self.vm.run();
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

        let program = self.compiler.compile(datums, tail);
        self.vm.append_program(program);
    }

    pub fn eval_str(&mut self, input: &str) -> Result<Value, LispErr> {
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

    pub fn bind_global(&mut self, name: Symbol, val: Value) {
        self.compiler.bind_global(name);
        self.vm.add_global(val);
    }

    pub fn run(&mut self) {
        if let Err(err) = self.vm.run() {
            println!("Err: {}", err);
        }
    }
}
