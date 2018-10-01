use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use ::Datum;
use ::LispFn;
use ::LispErr::*;
use ::LispResult;
use ::Arity;

use ::builtin::*;
use compiler::vm::VM;

fn println(vs: &mut [Datum], vm: &VM) -> LispResult {
    let mut output = vm.output.borrow_mut();
    for v in vs.iter() {
        match *v {
            Datum::String(ref x) => {
                if let Err(_err) = write!(output, "{}", x) {
                    return Err(IOError)
                }
            },
            ref other => {
                if let Err(_err) = write!(output, "{}", other.to_string(&vm.symbol_table.borrow())) {
                    return Err(IOError)
                }
            }
        };
    }
    if let Err(_err) = write!(output, "\n") {
        return Err(IOError)
    }
    Ok(Datum::Undefined)
}

fn print(vs: &mut [Datum], vm: &VM) -> LispResult {
    let mut output = vm.output.borrow_mut();
    for v in vs.iter() {
        match *v {
            Datum::String(ref x) => {
                if let Err(_err) = write!(output, "{}", x) {
                    return Err(IOError)
                }
            },
            ref other => {
                if let Err(_err) = write!(output, "{}", other.to_string(&vm.symbol_table.borrow())) {
                    return Err(IOError)
                }
            }
        };
    }
    Ok(Datum::Undefined)
}

fn inspect(a: Datum, vm: &VM) -> LispResult {
    match writeln!(vm.output.borrow_mut(), "{:?}", a) {
        Err(_err) => Err(IOError),
        Ok(_) => Ok(Datum::Undefined)
    }
}

fn file_read(a: Datum, _vm: &VM) -> LispResult {
    if let Datum::String(ref b) = a {
        match File::open(b) {
            Ok(ref mut file) => {
                let mut result = String::new();
                match file.read_to_string(&mut result) {
                    Ok(_) => return Ok(Datum::String(result)),
                    Err(_) => return Err(IOError),
                };
            },
            Err(_) => return Err(IOError)
        }
    }
    Err(InvalidTypeOfArguments)
}

// fn apply(vs: &mut [Datum], _vm: &VM) -> LispResult {
//     let f = vs[0].clone();
//     let args = vs[1].clone().as_list()?;
//     Ok(eval.full_apply(f, args, env_ref))
// }

// fn read(vs: &mut [Datum], _eval: &mut Evaluator, _env_ref: EnvRef) -> LispResult {
//     let arg = vs.get(0).unwrap();
//     if let Datum::String(ref input) = *arg {
//         let result = parser::parse_datum(input.as_ref());
//         Ok(result)
//     } else {
//         Err(InvalidTypeOfArguments)
//     }
// }

// TODO: Fix the way environments are handled here
// fn eval(vs: &mut [Datum], eval: &mut Evaluator, _env_ref: EnvRef) -> LispResult {
//     let env = eval.root_env.clone();
//     eval.eval_datum(vs[0].clone(), env)
// }

pub fn load(hm: &mut HashMap<String, LispFn>) {
    register_var(hm, "println", println, Arity::Min(0));
    register_var(hm, "print", print, Arity::Min(0));
    register1(hm, "inspect", inspect);
    register1(hm, "file-read", file_read);
    // register(hm, "apply", apply, Arity::Exact(2));
    // register(hm, "read", read, Arity::Exact(1));
    // register(hm, "eval", eval, Arity::Exact(1));
}
