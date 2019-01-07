use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::evaluator::Evaluator;
use crate::Datum;

pub fn run(stdlib: bool) {
    let mut rl = Editor::<()>::new();
    let mut eval = Evaluator::new(Rc::new(RefCell::new(io::stdout())), stdlib);
    let mut res_index = 0;

    if let Err(_) = rl.load_history("history.txt") {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match eval.eval_str(&line) {
                    Ok(res) => {
                        if res != Datum::Undefined {
                            let name = format!("${}", res_index);
                            println!("{} = {}", name, res.to_string(&eval.symbol_table.borrow()));
                            eval.bind_global(name, res.clone());
                            res_index += 1;
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
