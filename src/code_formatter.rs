use std::iter::Peekable;
use std::result::Result;

use crate::lexer::{Lexer, Literal, Position, Token};
use crate::LispError;

#[derive(Debug)]
pub struct FormatterError {
    start: Position,
    end: Position,
    error: FormatterErrorType,
    source: Option<String>,
}

#[derive(Debug)]
pub enum FormatterErrorType {
    UnexpectedEndOfInput,
    UnexpectedToken,
    UnexpectedDot,
    UnbalancedBracket,
    InvalidDottedList,
    InvalidNumberLiteral,
    InvalidInfixList,
}

use self::FormatterErrorType::*;

impl From<FormatterError> for LispError {
    fn from(error: FormatterError) -> Self {
        LispError::FormatterError(error)
    }
}

pub struct Formatter<'a> {
    input: Peekable<Lexer<'a>>,
    end: Position,
    source: Option<String>,
    decorator_stack: Vec<String>,
}

// The first string if for "decorators"
// like '#' and '&' for lists
// and quoting / quasi-quoting characters
#[derive(Clone, Debug)]
pub enum Element {
    List(String, Vec<Element>, Option<String>, Position, Position),
    DottedList(
        String,
        Vec<Element>,
        Box<Element>,
        Option<String>,
        Position,
        Position,
    ),
    Literal(String, String, Option<String>, Position, Position),
    Comment(String, Position, Position),
}

impl Element {
    pub fn len(&self) -> usize {
        match self {
            Element::List(dec, els, _, _, _) => {
                if els.is_empty() {
                    dec.len() + 2
                } else {
                    let inner: usize = els.iter().map(|e| e.len()).sum();
                    dec.len() + inner + 2 + els.len() - 1
                }
            }
            Element::DottedList(dec, els, tail, _, _, _) => {
                // TODO: Is it possible to have a dotted list with els = []?
                let inner: usize = els.iter().map(|e| e.len()).sum();
                dec.len() + inner + tail.len() + 4 + els.len() - 1
            }
            Element::Literal(dec, body, _, _, _) => dec.len() + body.len(),
            Element::Comment(body, _, _) => {
                // 1 = leading ';'
                body.len() + 1
            }
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            Element::List(_, els, _, _, _) => 1 + els.iter().map(|e| e.depth()).max().unwrap_or(0),
            Element::DottedList(_, els, tail, _, _, _) => {
                let max: usize = els.iter().map(|e| e.depth()).max().unwrap_or(0);
                1 + usize::max(max, tail.depth())
            }
            Element::Literal(_, _, _, _, _) => 1,
            Element::Comment(_, _, _) => 1,
        }
    }

    pub fn is_multiline(&self) -> bool {
        let (start, end) = match self {
            Element::List(_, _, _, start, end) => (start, end),
            Element::DottedList(_, _, _, _, start, end) => (start, end),
            Element::Literal(_, _, _, start, end) => (start, end),
            Element::Comment(_, start, end) => (start, end),
        };

        start.0 > end.0
    }
}

impl<'a> Formatter<'a> {
    pub fn from_string(string: &'a String, source: Option<String>) -> Self {
        let lexer = Lexer::from_string(string, source.clone());
        Formatter {
            input: lexer.peekable(),
            end: Position(0, 0),
            source,
            decorator_stack: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>, LispError> {
        loop {
            if let Some(lit_err) = self.input.next() {
                let lit = lit_err?;
                self.end = lit.end;
                return Ok(Some(lit));
            } else {
                return Ok(None);
            }
        }
    }

    pub fn all(&mut self) -> Result<Vec<Element>, LispError> {
        let mut res = Vec::new();
        while let Some(el) = self.next_element()? {
            res.push(el);
        }

        Ok(res)
    }

    fn make_error(&self, start: Position, typ: FormatterErrorType) -> FormatterError {
        FormatterError {
            start,
            end: self.end,
            source: self.source.clone(),
            error: typ,
        }
    }

    fn pop_decorators(&mut self) -> String {
        let decorators: Vec<String> = self.decorator_stack.iter().rev().cloned().collect();
        let res = decorators.concat();
        self.decorator_stack.clear();
        res
    }

    pub fn next_element(&mut self) -> Result<Option<Element>, LispError> {
        while let Some(t) = self.next()? {
            match t.literal {
                Literal::Comment(text) => {
                    // TODO: Make sure there are no decorators left
                    return Ok(Some(Element::Comment(text, t.start, t.end)));
                }
                Literal::Bool(b) => {
                    let string = if b {
                        String::from("#t")
                    } else {
                        String::from("#f")
                    };

                    let dec = self.pop_decorators();
                    return Ok(Some(Element::Literal(dec, string, None, t.start, t.end)));
                }
                Literal::Char(c) => {
                    let string = match c {
                        ' ' => String::from("#\\space"),
                        '\n' => String::from("#\\newline"),
                        other => format!("#\\{}", other),
                    };

                    let dec = self.pop_decorators();
                    return Ok(Some(Element::Literal(dec, string, None, t.start, t.end)));
                }
                Literal::String(mut s) => {
                    // Undo string escapes
                    s = s.replace("\n", "\\n");
                    s = s.replace("\r", "\\r");
                    s = s.replace("\t", "\\t");
                    s = s.replace("\"", "\\\"");
                    s = s.replace("\\", "\\\\");

                    let string = format!("\"{}\"", s);
                    let dec = self.pop_decorators();
                    return Ok(Some(Element::Literal(dec, string, None, t.start, t.end)));
                }
                Literal::Identifier(string) => {
                    let dec = self.pop_decorators();
                    return Ok(Some(Element::Literal(dec, string, None, t.start, t.end)));
                }
                // TODO: Check if `body` contains `_`
                // and reformat it so that digits are separated into groups of three
                Literal::Number(sign, base, body, sign_given, base_given) => {
                    let mut string = String::new();

                    if base_given {
                        match base {
                            2 => string.push_str("#b"),
                            8 => string.push_str("#o"),
                            16 => string.push_str("#x"),
                            _ => (),
                        }
                    }

                    if sign_given {
                        if sign {
                            string.push_str("+");
                        } else {
                            string.push_str("-");
                        }
                    }

                    string.push_str(&body);

                    let dec = self.pop_decorators();
                    return Ok(Some(Element::Literal(dec, string, None, t.start, t.end)));
                }
                Literal::LRoundBracket => {
                    let dec = self.pop_decorators();
                    return Ok(Some(self.process_list(
                        dec,
                        t.start,
                        Literal::RRoundBracket,
                    )?));
                }
                Literal::LSquareBracket => {
                    let dec = self.pop_decorators();
                    return Ok(Some(self.process_list(
                        dec,
                        t.start,
                        Literal::RSquareBracket,
                    )?));
                }
                Literal::HashLRoundBracket => {
                    self.decorator_stack.push(String::from("#"));
                    let dec = self.pop_decorators();
                    return Ok(Some(self.process_simple_list(
                        dec,
                        t.start,
                        Literal::RRoundBracket,
                    )?));
                }
                Literal::HashLSquareBracket => {
                    self.decorator_stack.push(String::from("#"));
                    let dec = self.pop_decorators();
                    return Ok(Some(self.process_simple_list(
                        dec,
                        t.start,
                        Literal::RSquareBracket,
                    )?));
                }
                Literal::AmpersandLRoundBracket => {
                    self.decorator_stack.push(String::from("&"));
                    let dec = self.pop_decorators();
                    return Ok(Some(self.process_simple_list(
                        dec,
                        t.start,
                        Literal::RRoundBracket,
                    )?));
                }
                Literal::AmpersandLSquareBracket => {
                    self.decorator_stack.push(String::from("&"));
                    let dec = self.pop_decorators();
                    return Ok(Some(self.process_simple_list(
                        dec,
                        t.start,
                        Literal::RSquareBracket,
                    )?));
                }
                Literal::LCurlyBracket => return Err(self.make_error(t.start, UnexpectedToken))?,
                Literal::RRoundBracket => return Err(self.make_error(t.start, UnbalancedBracket))?,
                Literal::RSquareBracket => return Err(self.make_error(t.start, UnbalancedBracket))?,
                Literal::RCurlyBracket => return Err(self.make_error(t.start, UnbalancedBracket))?,
                Literal::Quote => self.decorator_stack.push(String::from("'")),
                Literal::Quasiquote => self.decorator_stack.push(String::from("`")),
                Literal::Unquote => self.decorator_stack.push(String::from(",")),
                Literal::UnquoteSplicing => self.decorator_stack.push(String::from(",@")),
                _ => {
                    return Err(FormatterError {
                        start: t.start,
                        end: t.end,
                        source: self.source.clone(),
                        error: UnexpectedToken,
                    })?;
                }
            }
        }

        Ok(None)
    }

    fn is_peek_none(&mut self) -> bool {
        self.input.peek().is_none()
    }

    fn is_peek_eq(&mut self, literal: &Literal) -> Result<bool, LispError> {
        match self.input.peek() {
            None => Ok(false),
            Some(&Err(ref err)) => Err(err.clone())?,
            Some(&Ok(ref l)) => Ok(l.literal == *literal),
        }
    }

    fn process_simple_list(
        &mut self,
        dec: String,
        start: Position,
        closing: Literal,
    ) -> Result<Element, LispError> {
        let mut res = Vec::new();

        loop {
            if self.is_peek_none() {
                return Err(self.make_error(start, UnexpectedEndOfInput))?;
            }

            if self.is_peek_eq(&closing)? {
                self.next()?;
                break;
            } else if self.is_peek_eq(&Literal::Dot)? {
                return Err(self.make_error(start, UnexpectedDot))?;
            } else if let Some(n) = self.next_element()? {
                res.push(n);
            } else {
                return Err(self.make_error(start, UnexpectedEndOfInput))?;
            }
        }

        Ok(Element::List(dec, res, None, start, self.end))
    }

    fn process_list(
        &mut self,
        dec: String,
        start: Position,
        closing: Literal,
    ) -> Result<Element, LispError> {
        let mut res = Vec::new();

        loop {
            if self.is_peek_none() {
                return Err(self.make_error(start, UnexpectedEndOfInput))?;
            }

            if self.is_peek_eq(&closing)? {
                self.next()?;
                break;
            } else if self.is_peek_eq(&Literal::Dot)? {
                // skip dot
                self.next()?;

                let tail = match self.next_element()? {
                    Some(d) => d,
                    None => {
                        return Err(self.make_error(start, InvalidDottedList))?;
                    }
                };

                if self.is_peek_eq(&closing)? {
                    self.next()?;
                } else {
                    return Err(self.make_error(start, InvalidDottedList))?;
                }

                return Ok(Element::DottedList(
                    dec,
                    res,
                    Box::new(tail),
                    None,
                    start,
                    self.end,
                ));
            } else if let Some(n) = self.next_element()? {
                res.push(n);
            } else {
                panic!("Unexpected end of input")
            }
        }

        Ok(Element::List(dec, res, None, start, self.end))
    }
}

pub struct PrettyPrinter {
    indentation: usize,
}

impl PrettyPrinter {
    pub fn new() -> PrettyPrinter {
        PrettyPrinter { indentation: 0 }
    }

    pub fn print(&mut self, el: &Element, indent: bool, newline: bool) {
        if indent {
            for i in 0..self.indentation {
                print!(" ");
            }
        }

        match el {
            Element::List(dec, els, _, start, end) => {
                let split = el.depth() > 4 || el.len() > 60;

                print!("{}", dec);
                print!("(");

                if els.is_empty() {
                    print!(")");
                } else {
                    self.indentation += 2;
                    if els.len() == 1 {
                        self.print(&els[0], false, false);
                    } else {
                        self.print(&els[0], false, split);
                    }

                    for (idx, el) in els.iter().enumerate().skip(1) {
                        if !split {
                            print!(" ");
                        }

                        if idx == els.len() - 1 {
                            // self.print(el, true, false);
                            self.print(el, split, false);
                        } else {
                            // self.print(el, true, split);
                            self.print(el, split, split);
                        }
                    }
                    if newline {
                        println!(")");
                    } else {
                        print!(")");
                    }

                    self.indentation -= 2;
                }
            }
            Element::DottedList(dec, els, tail, _, start, end) => {
                print!("{}", dec);
                print!("(");
                if els.is_empty() {
                    print!(")");
                } else {
                    self.indentation += 2;
                    self.print(&els[0], false, true);

                    for el in els.iter().skip(1) {
                        print!(" ");
                        self.print(el, true, true);
                    }

                    print!(" . ");
                    self.indentation -= 2;
                    println!(")");
                }
            }
            Element::Literal(dec, body, _, start, end) => {
                print!("{}", dec);
                if newline {
                    println!("{}", body);
                } else {
                    print!("{}", body);
                }
            }
            Element::Comment(body, start, end) => {
                println!(";{}", body);
            }
        }
    }
}
