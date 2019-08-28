use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::symbol_table::Symbol;
use crate::vm::VM;
use crate::Value;

pub fn run(stdlib: bool) {
    let mut rl = Editor::<()>::new();
    let mut vm = VM::new(Rc::new(RefCell::new(io::stdout())));
    if stdlib {
        vm.eval_stdlib();
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
                match vm.eval_str(&line) {
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
