use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::evaluator::Evaluator;

pub fn run(stdlib: bool) {
    let mut rl = Editor::<()>::new();
    let mut eval = Evaluator::new(Rc::new(RefCell::new(io::stdout())), stdlib);
    let mut res_index = 0;

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match eval.eval_str(&line) {
                    Ok(res) => {
                        // TODO: the repl needs a symbol table for this
                        // if res != Value::Undefined {
                        //     let name = format!("${}", res_index);
                        // println!("{} = {}", name, res.to_string(&eval.symbol_table.borrow()));
                        //     eval.bind_global(&name, res.clone());
                        //     res_index += 1;
                        // }
                        println!("#> {}", res);
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
