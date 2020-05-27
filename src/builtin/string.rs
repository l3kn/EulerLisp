#![allow(clippy::needless_pass_by_value)]

use std::convert::TryInto;

use crate::builtin::*;
use crate::vm::VM;
use crate::LispError::*;
use crate::{Arity, LispResult, Value};

fn string_get(s: Value, i: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = s.try_into()?;
    let index: usize = i.try_into()?;
    let c = string.chars().nth(index).unwrap();
    Ok(Value::Char(c))
}

fn string_bytes(s: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = s.try_into()?;
    let bytes = string
        .as_bytes()
        .iter()
        .map(|b| Value::Integer(*b as isize))
        .collect();
    Ok(Value::make_list_from_vec(bytes))
}

fn string_length(s: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = s.try_into()?;
    Ok(Value::Integer(string.len() as isize))
}

fn string_to_number(s: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = s.try_into()?;
    match string.parse::<isize>() {
        Ok(i) => Ok(Value::Integer(i)),
        Err(_) => Err(InvalidTypeOfArguments),
    }
}

fn string_split(splitter: Value, string: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = string.try_into()?;
    let splitter: String = splitter.try_into()?;
    let lines: Vec<Value> = string
        .split(&splitter)
        .map(|l| Value::String(l.to_string()))
        .collect();

    Ok(Value::make_list_from_vec(lines))
}

fn string_str(vs: &mut [Value], _vm: &VM) -> LispResult<Value> {
    let mut result = String::new();

    for v in vs.into_iter() {
        match v {
            &mut Value::String(ref s) => result += s,
            other => result += &other.to_string(),
        }
    }
    Ok(Value::String(result))
}

fn string_trim(s: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = s.try_into()?;
    Ok(Value::String(string.trim().to_string()))
}

fn string_to_chars(s: Value, _vm: &VM) -> LispResult<Value> {
    let string: String = s.try_into()?;
    Ok(Value::make_list_from_vec(
        string.chars().map(Value::Char).collect(),
    ))
}

fn chars_to_string(c: Value, _vm: &VM) -> LispResult<Value> {
    let pair = c.as_pair()?;
    let chars = pair.collect_list()?;

    let s: LispResult<String> = chars
        .into_iter()
        .map(|c| -> LispResult<char> { c.try_into() })
        .collect();
    Ok(Value::String(s?))
}

fn char_to_integer(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;
    Ok(Value::Integer(c as isize))
}

fn char_to_digit(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;

    if c.is_ascii_digit() {
        // 48 is ASCII of 0
        Ok(Value::Integer(c as isize - 48))
    } else {
        Err(InvalidNumberOfArguments)
    }
}

fn char_is_numeric(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;
    Ok(Value::Bool(c.is_ascii_digit()))
}

fn char_is_alphabetic(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;
    Ok(Value::Bool(c.is_ascii_alphabetic()))
}

fn char_is_whitespace(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;
    Ok(Value::Bool(c.is_ascii_whitespace()))
}

fn char_is_upper_case(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;
    Ok(Value::Bool(c.is_ascii_uppercase()))
}

fn char_is_lower_case(c: Value, _vm: &VM) -> LispResult<Value> {
    let c: char = c.try_into()?;
    Ok(Value::Bool(c.is_ascii_lowercase()))
}

pub fn load(reg: &mut dyn BuiltinRegistry) {
    reg.register2("string-get", string_get);
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
