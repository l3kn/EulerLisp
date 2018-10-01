use std::collections::HashMap;

use ::Datum;
use ::LispFn;
use ::LispErr;
use ::LispErr::*;
use ::LispResult;
use ::Arity;
use ::builtin::*;
use compiler::vm::VM;

fn string_bytes(s: Datum, _vm: &VM) -> LispResult {
    let string = s.as_string()?;
    let bytes = string.as_bytes().iter().map(|b| Datum::Integer(*b as isize)).collect();
    Ok(Datum::make_list_from_vec(bytes))
}

fn string_length(s: Datum, _vm: &VM) -> LispResult {
    let string = s.as_string()?;
    Ok(Datum::Integer(string.len() as isize))
}

fn string_to_number(s: Datum, _vm: &VM) -> LispResult {
    let string = s.as_string()?;
    match string.parse::<isize>() {
        Ok(i) => Ok(Datum::Integer(i)),
        Err(_) => Err(InvalidTypeOfArguments)
    }
}

fn string_split(splitter: Datum, string: Datum, _vm: &VM) -> LispResult {
    let string = string.as_string()?;
    let splitter = splitter.as_string()?;
    let lines: Vec<Datum> =
        string.split(&splitter)
        .map( |l| Datum::String(l.to_string()) )
        .collect();

    Ok(Datum::make_list_from_vec(lines))
}

fn string_str(vs: &mut [Datum], vm: &VM) -> LispResult {
    let mut result = String::new();

    for v in vs.into_iter() {
        match v {
            &mut Datum::String(ref s) => result += s,
            other => result += &other.to_string(&vm.symbol_table.borrow()),
        }
    }
    return Ok(Datum::String(result));
}

fn string_trim(s: Datum, _vm: &VM) -> LispResult {
    let string = s.as_string()?;
    Ok(Datum::String(string.trim().to_string()))
}

fn string_to_chars(s: Datum, _vm: &VM) -> LispResult {
    let string = s.as_string()?;
    Ok(Datum::make_list_from_vec(string.chars().map(|c| Datum::Char(c) ).collect()))
}

fn chars_to_string(c: Datum, _vm: &VM) -> LispResult {
    let pair = c.as_pair()?;
    let chars = pair.collect_list()?;

    let s: Result<String, LispErr> = chars.into_iter().map(|c| c.as_char()).collect();
    Ok(Datum::String(s?))
}

fn char_to_integer(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;
    Ok(Datum::Integer(c as isize))
}

fn char_to_digit(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;

    if c.is_ascii_digit() {
        // 48 is ASCII of 0
        Ok(Datum::Integer(c as isize - 48))
    } else {
        Err(InvalidNumberOfArguments)
    }
}

fn char_is_numeric(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_digit()))
}

fn char_is_alphabetic(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_alphabetic()))
}

fn char_is_whitespace(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_whitespace()))
}

fn char_is_upper_case(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_uppercase()))
}

fn char_is_lower_case(c: Datum, _vm: &VM) -> LispResult {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_lowercase()))
}

pub fn load(hm: &mut HashMap<String, LispFn>) {
    register1(hm, "string-bytes", string_bytes);
    register1(hm, "string-length", string_length);
    register1(hm, "string-trim", string_trim);
    register1(hm, "string->number", string_to_number);
    register1(hm, "string->chars", string_to_chars);
    register1(hm, "chars->string", chars_to_string);
    register2(hm, "string-split", string_split);
    register_var(hm, "str", string_str, Arity::Min(0));
    register1(hm, "char-alphabetic?", char_is_alphabetic);
    register1(hm, "char-numeric?", char_is_numeric);
    register1(hm, "char-whitespace?", char_is_whitespace);
    register1(hm, "char-upper-case?", char_is_upper_case);
    register1(hm, "char-lower-case?", char_is_lower_case);
    register1(hm, "char->integer", char_to_integer);
    register1(hm, "char->digit", char_to_digit);
}
