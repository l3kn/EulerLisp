use std::cell::RefCell;
use std::rc::Rc;
use std::fs;
use std::fs::File;
use std::io::{Read, Write};

use crate::{Datum, LispErr};
use crate::vm::VM;

/// Wrapper around a compiler and VM
/// to allow easy execution of programs
pub struct Evaluator {
    pub vm: VM,
}

impl Evaluator {
    pub fn new(output: Rc<RefCell<Write>>, stdlib: bool) -> Self {
        let vm = VM::new(output);
        let mut eval = Evaluator { vm };
        if stdlib { eval.load_stdlib(); }

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
    }

    pub fn load_file(&mut self, path: &str, tail: bool) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input).expect("Could not read file");

        self.vm.load_str(&input[..], tail, Some(path.to_string()));
    }

    pub fn eval_str(&mut self, input: &str) -> Result<Datum, LispErr> {
        // To make the REPL work, jump to the end of the old program
        // every time new code is evaluated
        let start = self.vm.bytecode.len();
        self.vm.load_str(input, false, None);

        // TODO: Find a more elegant way to do this.
        // The problem occurs when `input` is e.g. (defcons ...),
        // so that no new instructions are added
        if start == self.vm.bytecode.len() {
            Ok(Datum::Undefined)
        } else {
            self.vm.set_pc(start as usize);
            self.run();
            Ok(self.vm.val.take())
        }
    }

    pub fn bind_global(&mut self, name: &str, val: Datum) {
        self.vm.bind_global(name, val);
    }

    pub fn run(&mut self) {
        if let Err(err) = self.vm.run() {
            println!("Err: {}", err);
        }
    }
}
