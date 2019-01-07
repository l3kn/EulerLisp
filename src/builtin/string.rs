use crate::{Arity, Datum, LispErr, LispResult};
use crate::vm::VM;
use crate::builtin::*;
use crate::LispErr::*;

fn string_bytes(s: Datum, _vm: &VM) -> LispResult<Datum> {
    let string = s.as_string()?;
    let bytes = string
        .as_bytes()
        .iter()
        .map(|b| Datum::Integer(*b as isize))
        .collect();
    Ok(Datum::make_list_from_vec(bytes))
}

fn string_length(s: Datum, _vm: &VM) -> LispResult<Datum> {
    let string = s.as_string()?;
    Ok(Datum::Integer(string.len() as isize))
}

fn string_to_number(s: Datum, _vm: &VM) -> LispResult<Datum> {
    let string = s.as_string()?;
    match string.parse::<isize>() {
        Ok(i) => Ok(Datum::Integer(i)),
        Err(_) => Err(InvalidTypeOfArguments),
    }
}

fn string_split(splitter: Datum, string: Datum, _vm: &VM) -> LispResult<Datum> {
    let string = string.as_string()?;
    let splitter = splitter.as_string()?;
    let lines: Vec<Datum> = string
        .split(&splitter)
        .map(|l| Datum::String(l.to_string()))
        .collect();

    Ok(Datum::make_list_from_vec(lines))
}

fn string_str(vs: &mut [Datum], vm: &VM) -> LispResult<Datum> {
    let mut result = String::new();

    for v in vs.into_iter() {
        match v {
            &mut Datum::String(ref s) => result += s,
            other => result += &other.to_string(&vm.symbol_table.borrow()),
        }
    }
    return Ok(Datum::String(result));
}

fn string_trim(s: Datum, _vm: &VM) -> LispResult<Datum> {
    let string = s.as_string()?;
    Ok(Datum::String(string.trim().to_string()))
}

fn string_to_chars(s: Datum, _vm: &VM) -> LispResult<Datum> {
    let string = s.as_string()?;
    Ok(Datum::make_list_from_vec(
        string.chars().map(|c| Datum::Char(c)).collect(),
    ))
}

fn chars_to_string(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let pair = c.as_pair()?;
    let chars = pair.collect_list()?;

    let s: Result<String, LispErr> = chars.into_iter().map(|c| c.as_char()).collect();
    Ok(Datum::String(s?))
}

fn char_to_integer(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Integer(c as isize))
}

fn char_to_digit(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let c = c.as_char()?;

    if c.is_ascii_digit() {
        // 48 is ASCII of 0
        Ok(Datum::Integer(c as isize - 48))
    } else {
        Err(InvalidNumberOfArguments)
    }
}

fn char_is_numeric(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_digit()))
}

fn char_is_alphabetic(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_alphabetic()))
}

fn char_is_whitespace(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_whitespace()))
}

fn char_is_upper_case(c: Datum, _vm: &VM) -> LispResult<Datum> {
    let c = c.as_char()?;
    Ok(Datum::Bool(c.is_ascii_uppercase()))
}

fn char_is_lower_case(c: Datum, _vm: &VM) -> LispResult<Datum> {
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
