#![allow(clippy::needless_pass_by_value)]

use std::fs::File;
use std::io::Read;

use crate::{Arity, Value, LispResult};
use crate::LispErr::*;

use crate::builtin::*;
use crate::vm::VM;

fn println(vs: &mut [Value], vm: &VM) -> LispResult<Value> {
    let mut output = vm.output.borrow_mut();
    for v in vs.iter() {
        match *v {
            Value::String(ref x) => {
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
    Ok(Value::Undefined)
}

fn print(vs: &mut [Value], vm: &VM) -> LispResult<Value> {
    let mut output = vm.output.borrow_mut();
    for v in vs.iter() {
        match *v {
            Value::String(ref x) => {
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
    Ok(Value::Undefined)
}

fn inspect(a: Value, vm: &VM) -> LispResult<Value> {
    match writeln!(vm.output.borrow_mut(), "{:?}", a) {
        Err(_err) => Err(IOError),
        Ok(_) => Ok(Value::Undefined),
    }
}

fn file_read(a: Value, _vm: &VM) -> LispResult<Value> {
    if let Value::String(ref b) = a {
        match File::open(b) {
            Ok(ref mut file) => {
                let mut result = String::new();
                match file.read_to_string(&mut result) {
                    Ok(_) => return Ok(Value::String(result)),
                    Err(_) => return Err(IOError),
                };
            }
            Err(_) => return Err(IOError),
        }
    }
    Err(InvalidTypeOfArguments)
}

// fn apply(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
//     let f = vs[0].clone();
//     let args = vs[1].clone().as_list()?;
//     Ok(eval.full_apply(f, args, env_ref))
// }

// fn read(vs: &mut [Value], _eval: &mut Evaluator, _env_ref: EnvRef) -> LispResult<Value> {
//     let arg = vs.get(0).unwrap();
//     if let Value::String(ref input) = *arg {
//         let result = parser::parse_datum(input.as_ref());
//         Ok(result)
//     } else {
//         Err(InvalidTypeOfArguments)
//     }
// }

// TODO: Fix the way environments are handled here
// fn eval(vs: &mut [Value], eval: &mut Evaluator, _env_ref: EnvRef) -> LispResult<Value> {
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
