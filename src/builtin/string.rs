#![allow(clippy::needless_pass_by_value)]

use crate::{Arity, Datum, LispErr, LispResult};
use crate::vm::OutputRef;
use crate::symbol_table::SymbolTable;
use crate::heap::Heap;
use crate::builtin::*;
use crate::LispErr::*;

fn string_bytes(s: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let string = match s {
        Datum::String(ptr) => heap.get_string(ptr),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };

    let bytes = string
        .as_bytes()
        .iter()
        .map(|b| Datum::Integer(*b as isize))
        .collect();
    Ok(heap.make_list_from_vec(bytes))
}

fn string_length(s: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let string = match s {
        Datum::String(ptr) => heap.get_string(ptr),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    Ok(Datum::Integer(string.len() as isize))
}

fn string_to_number(s: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let string = match s {
        Datum::String(ptr) => heap.get_string(ptr),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    match string.parse::<isize>() {
        Ok(i) => Ok(Datum::Integer(i)),
        Err(_) => Err(InvalidTypeOfArguments),
    }
}

fn string_split(splitter: Datum, string: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let string = match string {
        Datum::String(ptr) => heap.get_string(ptr).clone(),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    let splitter = match splitter {
        Datum::String(ptr) => heap.get_string(ptr).clone(),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    let lines: Vec<Datum> = string
        .split(&splitter)
        .map(|l| heap.make_string(l.to_string()))
        .collect();

    Ok(heap.make_list_from_vec(lines))
}

fn string_str(vs: &mut [Datum], _out: &OutputRef, st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let mut result = String::new();

    for v in vs.into_iter() {
        match v {
            &mut Datum::String(ptr) => result += heap.get_string(ptr),
            other => result += &other.to_string(st, heap, false),
        }
    }
    Ok(heap.make_string(result))
}

fn string_trim(s: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let string = match s {
        Datum::String(ptr) => heap.get_string(ptr),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    Ok(heap.make_string(string.trim().to_string()))
}

fn string_to_chars(s: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    let string = match s {
        Datum::String(ptr) => heap.get_string(ptr),
        _other => return Err(LispErr::InvalidTypeOfArguments)
    };
    Ok(heap.make_list_from_vec(string.chars().map(Datum::Char).collect()))
}

fn chars_to_string(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, heap: &mut Heap) -> LispResult<Datum> {
    match c {
        Datum::Pair(ptr) => {
            let chars = heap.get_pair_list(ptr)?;
            let s: Result<String, LispErr> = chars.into_iter().map(|c| c.as_char()).collect();
            Ok(heap.make_string(s?))
        }
        Datum::Nil => Ok(heap.make_string(String::new())),
        _ => Err(InvalidTypeOfArguments),
    }
}

fn char_to_integer(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Integer(c as isize))
}

fn char_to_digit(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;

    if c.is_ascii_digit() {
        // 48 is ASCII of 0
        Ok(Datum::Integer(c as isize - 48))
    } else {
        Err(InvalidNumberOfArguments)
    }
}

fn char_is_numeric(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_digit()))
}

fn char_is_alphabetic(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_alphabetic()))
}

fn char_is_whitespace(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_whitespace()))
}

fn char_is_upper_case(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_uppercase()))
}

fn char_is_lower_case(c: Datum, _out: &OutputRef, _st: &mut SymbolTable, _heap: &mut Heap) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_lowercase()))
}

pub fn load(reg: &mut BuiltinRegistry) {
    reg.register1("string-bytes", string_bytes);
    reg.register1("string-length", string_length);
    reg.register1("string-trim", string_trim);
    reg.register1("string->number", string_to_number);
    reg.register1("string->chars", string_to_chars);
    reg.register1("chars->string", chars_to_string);
    reg.register2("string-split", string_split);
    reg.register_var("str", string_str, Arity::Min(0));
    reg.register1("char-alphabetic?", char_is_alphabetic);
    reg.register1("char-numeric?", char_is_numeric);
    reg.register1("char-whitespace?", char_is_whitespace);
    reg.register1("char-upper-case?", char_is_upper_case);
    reg.register1("char-lower-case?", char_is_lower_case);
    reg.register1("char->integer", char_to_integer);
    reg.register1("char->digit", char_to_digit);
}
