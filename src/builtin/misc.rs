#![allow(clippy::needless_pass_by_value)]

use std::fs::File;
use std::io::Read;

use crate::{Arity, Datum, LispResult};
use crate::LispErr::*;

use crate::builtin::*;
use crate::vm::OutputRef;
use crate::symbol_table::SymbolTable;
use crate::heap::Heap;

fn println(vs: &mut [Datum], out: &OutputRef, st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let mut output = out.borrow_mut();
    for v in vs.iter() {
        if let Err(_err) = write!(output, "{}", v.to_string(st, heap, false))
        {
            return Err(IOError);
        }
    }
    if let Err(_err) = writeln!(output) {
        return Err(IOError);
    }
    Ok(Datum::Undefined)
}

fn print(vs: &mut [Datum], out: &OutputRef, st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let mut output = out.borrow_mut();
    for v in vs.iter() {
        if let Err(_err) = write!(output, "{}", v.to_string(st, heap, false)) {
            return Err(IOError);
        }
    }
    Ok(Datum::Undefined)
}

fn inspect(a: Datum, out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    match writeln!(out.borrow_mut(), "{:?}", a) {
        Err(_err) => Err(IOError),
        Ok(_) => Ok(Datum::Undefined),
    }
}

fn file_read(a: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    if let Datum::String(b) = a {
        let b = heap.get_string(b);
        match File::open(b) {
            Ok(ref mut file) => {
                let mut result = String::new();
                match file.read_to_string(&mut result) {
                    Ok(_) => Ok(heap.make_string(result)),
                    Err(_) => Err(IOError),
                }
            },
            Err(_) => Err(IOError)
        }
    } else {
        Err(InvalidTypeOfArguments)
    }
}

// fn apply(vs: &mut [Datum], _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
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
