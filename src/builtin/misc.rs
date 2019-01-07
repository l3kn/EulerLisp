#![allow(clippy::needless_pass_by_value)]

use std::fs::File;
use std::io::Read;

use crate::{Arity, Datum, LispResult};
use crate::LispErr::*;

use crate::builtin::*;
use crate::vm::VM;

fn println(vs: &mut [Datum], vm: &VM) -> LispResult<Datum> {
    let mut output = vm.output.borrow_mut();
    for v in vs.iter() {
        match *v {
            Datum::String(ref x) => {
                if let Err(_err) = write!(output, "{}", x) {
                    return Err(IOError);
                }
            }
            ref other => {
                if let Err(_err) = write!(output, "{}", other.to_string(&vm.symbol_table.borrow()))
                {
                    return Err(IOError);
                }
            }
        };
    }
    if let Err(_err) = writeln!(output) {
        return Err(IOError);
    }
    Ok(Datum::Undefined)
}

fn print(vs: &mut [Datum], vm: &VM) -> LispResult<Datum> {
    let mut output = vm.output.borrow_mut();
    for v in vs.iter() {
        match *v {
            Datum::String(ref x) => {
                if let Err(_err) = write!(output, "{}", x) {
                    return Err(IOError);
                }
            }
            ref other => {
                if let Err(_err) = write!(output, "{}", other.to_string(&vm.symbol_table.borrow()))
                {
                    return Err(IOError);
                }
            }
        };
    }
    Ok(Datum::Undefined)
}

fn inspect(a: Datum, vm: &VM) -> LispResult<Datum> {
    match writeln!(vm.output.borrow_mut(), "{:?}", a) {
        Err(_err) => Err(IOError),
        Ok(_) => Ok(Datum::Undefined),
    }
}

fn file_read(a: Datum, _vm: &VM) -> LispResult<Datum> {
    if let Datum::String(ref b) = a {
        match File::open(b) {
            Ok(ref mut file) => {
                let mut result = String::new();
                match file.read_to_string(&mut result) {
                    Ok(_) => return Ok(Datum::String(result)),
                    Err(_) => return Err(IOError),
                };
            }
            Err(_) => return Err(IOError),
        }
    }
    Err(InvalidTypeOfArguments)
}

// fn apply(vs: &mut [Datum], _vm: &VM) -> LispResult<Datum> {
//     let f = vs[0].clone();
//     let args = vs[1].clone().as_list()?;
//     Ok(eval.full_apply(f, args, env_ref))
// }

// fn read(vs: &mut [Datum], _eval: &mut Evaluator, _env_ref: EnvRef) -> LispResult<Datum> {
//     let arg = vs.get(0).unwrap();
//     if let Datum::String(ref input) = *arg {
//         let result = parser::parse_datum(input.as_ref());
//         Ok(result)
//     } else {
//         Err(InvalidTypeOfArguments)
//     }
// }

// TODO: Fix the way environments are handled here
// fn eval(vs: &mut [Datum], eval: &mut Evaluator, _env_ref: EnvRef) -> LispResult<Datum> {
//     let env = eval.root_env.clone();
//     eval.eval_datum(vs[0].clone(), env)
// }

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register_var("println", println, Arity::Min(0));
    reg.register_var("print", print, Arity::Min(0));
    reg.register1("inspect", inspect);
    reg.register1("file-read", file_read);
    // register("apply", apply, Arity::Exact(2));
    // register("read", read, Arity::Exact(1));
    // register("eval", eval, Arity::Exact(1));
}
