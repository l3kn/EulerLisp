use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{env, io};

use euler_lisp::code_formatter::{Formatter, PrettyPrinter};
use euler_lisp::debugger::Debugger;
use euler_lisp::vm::VM;
use euler_lisp::{doc, repl};

fn main() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Usage:");
        println!("  {} repl", args[0]);
        println!("  {} run <file>", args[0]);
        println!();
        println!("Flags:");
        println!("  --no-stdlib, don't load the stdlib on startup");
    } else {
        args.remove(0); // skip program name
        let command = args.remove(0);
        let use_stdlib = !args.iter().any(|x| *x == "--no-stdlib");
        let readline = args.iter().any(|x| *x == "-r" || *x == "--readline");
        match &command[..] {
            "fmt" => {
                let filename = args.get(0).expect("No filename provided").clone();

                let mut file = File::open(filename.clone()).expect("Could not open file");
                let mut input = String::new();
                file.read_to_string(&mut input)
                    .expect("Could not read file");

                let mut formatter = Formatter::from_string(input, Some(filename));
                let mut printer = PrettyPrinter::new();

                for el in &formatter.all().unwrap() {
                    printer.print(el, true, true);
                    println!()
                }
            }
            "repl" => {
                if readline {
                    repl::run_readline(use_stdlib);
                } else {
                    repl::run(use_stdlib);
                }
            }
            v @ "run" | v @ "doc" => {
                let mut filename = args.get(0).expect("No filename provided").clone();

                if v == "run" {
                    let mut vm = VM::new();
                    if use_stdlib {
                        vm.load_stdlib();
                    }
                    vm.load_file(&filename, true);
                } else {
                    doc::process_file(&filename);
                }
            }
            // "debug" => {
            //     let mut filename = args.get(0).expect("No filename provided").clone();

            //     if !(filename.ends_with(".scm") || filename.ends_with(".lisp")) {
            //         let problem = filename.parse::<isize>().unwrap();
            //         if let Some(problem_path) = find_file_for_problem(problem, true) {
            //             filename = problem_path.to_str().unwrap().to_string();
            //         } else {
            //             panic!(format!("Could not find file for problem {}", problem));
            //         }
            //     }

            //     let mut debugger = Debugger::new(use_stdlib);
            //     debugger.debug_file(&filename);
            // }
            other => println!("Unknown command \"{}\"", other),
        }
    }
}
