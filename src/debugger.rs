use std::rc::Rc;
use std::cell::RefCell;

use std::fs;
use std::fs::File;
use std::io::Read;

use compiler::{Compiler, Program};
use instruction::{Instruction, LabeledInstruction};
use symbol_table::SymbolTable;
use builtin::{self, BuiltinRegistry};
use parser::Parser;
use Datum;
use Expression;

/// Compile a program together with the stdlib
/// and then output its instructions
/// (not reduced to their byte format)
/// in a human readable form
pub struct Debugger {
    compiler: Compiler,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Datum>,
}

impl Debugger {
    pub fn new(stdlib: bool) -> Self {
        let symbol_table = SymbolTable::new();
        let st_ref = Rc::new(RefCell::new(symbol_table));

        let mut registry = BuiltinRegistry::new();
        builtin::load(&mut registry);

        let mut eval = Debugger {
            compiler: Compiler::new(st_ref.clone(), registry),
            symbol_table: st_ref,
            constants: Vec::new(),
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

        let mut parser = Parser::from_string(&full);

        // TODO: convert parser errors to lisp errors
        let mut datums: Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        // Ingnore the results,
        // just make sure the compiler contains all the macros & globals
        // from the stdlib
        let Program { constants, .. } = self.compiler.compile(datums, true);

        self.constants.extend(constants);
    }

    pub fn debug_file(&mut self, path: &str) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input).expect(
            "Could not read file",
        );

        let mut parser = Parser::from_string(&input);

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

        self.constants.extend(constants);

        println!("New Globals: {}", num_globals);
        println!("Instructions:");

        // TODO: Add some other way to debug files
        // as human readable instruction sequences
        for (i, e) in instructions.iter().enumerate() {
            self.prettyprint(e);
        }
    }

    pub fn prettyprint(&self, linst: &LabeledInstruction) {
        let st = self.symbol_table.borrow();
        let (inst, label) = linst;
        match *inst {
            Instruction::Constant(i) => {
                println!(
                    "    CONSTANT ${}",
                     self.constants[i as usize].to_string(&st)
                 );
            }
            Instruction::PushConstant(i) => {
                println!(
                    "    PUSH-CONSTANT ${}",
                    self.constants[i as usize].to_string(&st)
                );
            }
            _ => println!("    {}", inst),
        }

        if let Some(l) = label {
            println!("{}:", l);
        }
    }
}

