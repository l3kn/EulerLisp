use std::cell::RefCell;
use std::iter::Peekable;
use std::rc::Rc;
use std::result::Result;

use crate::lexer::{Lexer, Literal, Position, Token};
use crate::symbol_table::SymbolTable;
use crate::LispError;
use crate::Value;

#[derive(Debug)]
pub struct ParserError {
    start: Position,
    end: Position,
    error: ParserErrorType,
    source: Option<String>,
}

#[derive(Debug)]
pub enum ParserErrorType {
    UnexpectedEndOfInput,
    UnexpectedToken,
    UnexpectedDot,
    UnbalancedBracket,
    InvalidDottedList,
    InvalidNumberLiteral,
    InvalidInfixList,
}

use self::ParserErrorType::*;

impl From<ParserError> for LispError {
    fn from(error: ParserError) -> Self {
        LispError::ParserError(error)
    }
}

pub struct Parser {
    input: Peekable<Lexer>,
    end: Position,
    source: Option<String>,
    symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Parser {
    pub fn new(symbol_table: Rc<RefCell<SymbolTable>>) -> Self {
        let lexer: Lexer = Lexer::from_string("".to_string(), None);
        Self {
            input: lexer.peekable(),
            end: Position(0, 0),
            source: None,
            symbol_table,
        }
    }

    pub fn load_string(&mut self, string: String, source: Option<String>) {
        let lexer: Lexer = Lexer::from_string(string, source.clone());
        self.input = lexer.peekable();
        self.source = source;
    }

    /// Return the next non-comment token, or None, if there are no more tokens
    pub fn next(&mut self) -> Result<Option<Token>, LispError> {
        loop {
            if let Some(lit_err) = self.input.next() {
                let lit = lit_err?;
                self.end = lit.end;
                if let Literal::Comment(_) = lit.literal {
                    continue;
                }

                return Ok(Some(lit));
            } else {
                return Ok(None);
            }
        }
    }

    fn make_error(&self, start: Position, typ: ParserErrorType) -> ParserError {
        ParserError {
            start,
            end: self.end,
            source: self.source.clone(),
            error: typ,
        }
    }

    pub fn next_value(&mut self) -> Result<Option<Value>, LispError> {
        if let Some(t) = self.next()? {
            match t.literal {
                Literal::Bool(v) => Ok(Some(Value::Bool(v))),
                Literal::Char(v) => Ok(Some(Value::Char(v))),
                Literal::String(v) => Ok(Some(Value::String(v))),
                Literal::Identifier(v) => Ok(Some(Value::Symbol(
                    self.symbol_table.borrow_mut().insert(&v),
                ))),
                Literal::Number(sign, base, body, _sign_given, _base_given) => {
                    match isize::from_str_radix(&body.replace("_", ""), base as u32) {
                        Ok(i) => {
                            let number = if sign { i } else { -i };
                            Ok(Some(Value::Integer(number)))
                        }
                        Err(_err) => {
                            if base == 10 {
                                let num = body.parse::<f64>();
                                match num {
                                    Ok(i) => {
                                        let number = if sign { i } else { -i };
                                        Ok(Some(Value::Float(number)))
                                    }
                                    Err(_err) => {
                                        Err(self.make_error(t.start, InvalidNumberLiteral))?
                                    }
                                }
                            } else {
                                Err(self.make_error(t.start, InvalidNumberLiteral))?
                            }
                        }
                    }
                }
                Literal::LRoundBracket => Ok(Some(self.process_list(
                    t.start,
                    Literal::RRoundBracket,
                    false,
                )?)),
                Literal::LSquareBracket => Ok(Some(self.process_list(
                    t.start,
                    Literal::RSquareBracket,
                    false,
                )?)),
                Literal::HashLRoundBracket => Ok(Some(self.process_list(
                    t.start,
                    Literal::RRoundBracket,
                    true,
                )?)),
                Literal::HashLSquareBracket => Ok(Some(self.process_list(
                    t.start,
                    Literal::RSquareBracket,
                    true,
                )?)),
                // Literal::AmpersandLRoundBracke => {
                //     let body = self.process_simple_list(t.start, Literal::RRoundBracket)?;
                //     Ok(Some(self.convert_hole_lambda_to_lambda(body)))
                // }
                // Literal::AmpersandLSquareBracket => {
                //     let body = self.process_simple_list(t.start, Literal::RSquareBracket)?;
                //     Ok(Some(self.convert_hole_lambda_to_lambda(body)))
                // }
                Literal::LCurlyBracket => Err(self.make_error(t.start, UnexpectedToken))?,
                Literal::RRoundBracket => Err(self.make_error(t.start, UnbalancedBracket))?,
                Literal::RSquareBracket => Err(self.make_error(t.start, UnbalancedBracket))?,
                Literal::RCurlyBracket => Err(self.make_error(t.start, UnbalancedBracket))?,
                Literal::Quote => {
                    match self.next_value()? {
                        Some(d) => {
                            match d {
                                // TODO: Fix this, using next_value here is not good
                                // because it uses `make_pair` and that returns Nil for empty pairs
                                Value::Nil => Ok(Some(Value::Nil)),
                                other => Ok(Some(Value::make_list_from_vec(vec![
                                    self.make_symbol("quote"),
                                    other,
                                ]))),
                            }
                        }
                        None => Err(self.make_error(t.start, UnexpectedEndOfInput))?,
                    }
                }
                Literal::Quasiquote => match self.next_value()? {
                    Some(d) => Ok(Some(Value::make_list_from_vec(vec![
                        self.make_symbol("quasiquote"),
                        d,
                    ]))),
                    None => Err(self.make_error(t.start, UnexpectedEndOfInput))?,
                },
                Literal::Unquote => match self.next_value()? {
                    Some(d) => Ok(Some(Value::make_list_from_vec(vec![
                        self.make_symbol("unquote"),
                        d,
                    ]))),
                    None => Err(self.make_error(t.start, UnexpectedEndOfInput))?,
                },
                Literal::UnquoteSplicing => match self.next_value()? {
                    Some(d) => Ok(Some(Value::make_list_from_vec(vec![
                        self.make_symbol("unquote-splicing"),
                        d,
                    ]))),
                    None => Err(self.make_error(t.start, UnexpectedEndOfInput))?,
                },
                _ => {
                    // NOTE: The `?` is necessary to convert this error
                    // to a LispError
                    Err(ParserError {
                        start: t.start,
                        end: t.end,
                        source: self.source.clone(),
                        error: UnexpectedToken,
                    })?
                }
            }
        } else {
            Ok(None)
        }
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
        start: Position,
        closing: Literal,
    ) -> Result<Vec<Value>, LispError> {
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
            } else if let Some(n) = self.next_value()? {
                res.push(n);
            } else {
                return Err(self.make_error(start, UnexpectedEndOfInput))?;
            }
        }

        Ok(res)
    }

    fn process_list(
        &mut self,
        start: Position,
        closing: Literal,
        is_vector: bool,
    ) -> Result<Value, LispError> {
        let mut res = Vec::new();

        loop {
            if self.is_peek_none() {
                return Err(self.make_error(start, UnexpectedEndOfInput))?;
            }

            if self.is_peek_eq(&closing)? {
                self.next()?;
                break;
            } else if self.is_peek_eq(&Literal::Dot)? {
                if is_vector {
                    return Err(self.make_error(start, UnexpectedDot))?;
                }
                // skip dot
                self.next()?;

                let tail = match self.next_value()? {
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

                return Ok(Value::make_dotted_list_from_vec(res, tail));
            } else if let Some(n) = self.next_value()? {
                res.push(n);
            } else {
                panic!("Unexpected end of input")
            }
        }

        if is_vector {
            Ok(Value::make_vector_from_vec(res))
        } else if res.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(Value::make_list_from_vec(res))
        }
    }

    fn make_symbol(&mut self, sym: &str) -> Value {
        Value::Symbol(self.symbol_table.borrow_mut().insert(sym))
    }

    // fn find_max_hole(&mut self, datum: &Value) -> isize {
    //     let mut max = 0;
    //     match *datum {
    //         Value::List(ref elems) => {
    //             for d in elems {
    //                 let res = self.find_max_hole(&d);
    //                 if res > max {
    //                     max = res;
    //                 }
    //             }
    //         }
    //         Value::Symbol(ref id) => {
    //             let mut tmp = self.symbol_table.borrow().lookup(id);
    //             let first = &tmp[0];

    //             if *first == '&' {
    //                 let res = tmp[1..]
    //                     .parse::<isize>()
    //                     .expect("Could not parse hole index");
    //                 if res > max {
    //                     max = res
    //                 }
    //             }
    //         }
    //         _ => (),
    //     }
    //     max
    // }

    // fn convert_hole_lambda_to_lambda(&mut self, datums: Vec<Value>) -> Value {
    //     let body = Value::make_list_from_vec(datums);
    //     let max = self.find_max_hole(&body);

    //     let mut params = Vec::new();

    //     for i in 1..=max {
    //         let param = format!("&{}", i);
    //         params.push(self.make_symbol(&param));
    //     }

    //     Value::make_list_from_vec(vec![
    //         self.make_symbol("fn"),
    //         Value::make_list_from_vec(params),
    //         body,
    //     ])
    // }
}
