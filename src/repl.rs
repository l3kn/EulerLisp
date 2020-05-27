use std::io::{self, Write};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::symbol_table::Symbol;
use crate::vm::VM;
use crate::Value;

pub fn run(stdlib: bool) {
    let mut vm = VM::new();
    if stdlib {
        vm.load_stdlib();
    }

    let mut res_index = 0;

    let mut line = String::new();
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        match io::stdin().read_line(&mut line) {
            Ok(_n) => {
                match vm.load_str(&line, false, None) {
                    Ok(res) => {
                        // TODO: the repl needs a symbol table for this
                        if res != Value::Undefined {
                            let name = format!("${}", res_index);
                            vm.context.add_global(Symbol::intern(&name), res.clone());
                            println!("{}> {}", res_index, res);
                            res_index += 1;
                        } else {
                            println!("#> {}", res);
                        }
                    }
                    Err(msg) => println!("!! {}", msg),
                };
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

pub fn run_readline(stdlib: bool) {
    let mut rl = Editor::<()>::new();
    let mut vm = VM::new();
    if stdlib {
        vm.load_stdlib();
    }

    let mut res_index = 0;

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match vm.load_str(&line, false, None) {
                    Ok(res) => {
                        // TODO: the repl needs a symbol table for this
                        if res != Value::Undefined {
                            let name = format!("${}", res_index);
                            vm.context.add_global(Symbol::intern(&name), res.clone());
                            println!("{}> {}", res_index, res);
                            res_index += 1;
                        } else {
                            println!("#> {}", res);
                        }
                    }
                    Err(msg) => println!("!! {}", msg),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history("history.txt").unwrap();
}
