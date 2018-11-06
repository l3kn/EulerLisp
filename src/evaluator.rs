use std::rc::Rc;
use std::cell::RefCell;

use std::fs;
use std::fs::File;
use std::io::{Read, Write};

use compiler::{Compiler, Program};
use vm::VM;
use instruction::convert_instructions;
use symbol_table::SymbolTable;
use builtin::{self, BuiltinRegistry};
use parser::Parser;
use Datum;
use Expression;
use LispErr;

/// Wrapper around a compiler and VM
/// to allow easy execution of programs
pub struct Evaluator {
    compiler: Compiler,
    vm: VM,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Evaluator {
    pub fn new(output: Rc<RefCell<Write>>, stdlib: bool) -> Self {
        let symbol_table = SymbolTable::new();
        let st_ref = Rc::new(RefCell::new(symbol_table));

        let mut registry = BuiltinRegistry::new();
        builtin::load(&mut registry);

        let vm = VM::new(output, st_ref.clone(), registry.clone());

        let mut eval = Evaluator {
            compiler: Compiler::new(st_ref.clone(), registry),
            vm,
            symbol_table: st_ref,
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

        let start = self.vm.bytecode.len();
        self.load_str(&full[..], false);
        self.vm.set_pc(start as usize);
        self.run();
    }

    pub fn load_file(&mut self, path: &str) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input).expect(
            "Could not read file",
        );

        self.load_str(&input[..], true);
    }

    fn load_str(&mut self, input: &str, tail: bool) {
        let string = String::from(input);
        let mut parser = Parser::from_string(&string);

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

        self.vm.append_instructions(convert_instructions(instructions));
        self.vm.append_constants(constants);
        self.vm.reserve_global_vars(num_globals);
    }

    pub fn eval_str(&mut self, input: &str) -> Result<Datum, LispErr> {
        // To make the REPL work, jump to the end of the old program
        // every time new code is evaluated
        let start = self.vm.bytecode.len();
        self.load_str(input, false);

        // TODO: Find a more elegant way to do this.
        // The problem occurs when `input` is e.g. (defcons ...),
        // so that no new instructions are added
        if start != self.vm.bytecode.len() {
            self.vm.set_pc(start as usize);
            self.run();
            Ok(self.vm.val.take())
        } else {
            Ok(Datum::Undefined)
        }
    }

    pub fn bind_global(&mut self, name: String, val: Datum) {
        self.compiler.bind_global(name);
        self.vm.add_global(val);
    }

    pub fn run(&mut self) {
        if let Err(err) = self.vm.run() {
            println!("Err: {}", err);
        }
    }
}

