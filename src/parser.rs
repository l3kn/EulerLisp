use std::iter::Peekable;
use std::result::Result;

use lexer::{Lexer, LexerError, Literal, Position, Token};

use Expression;

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

#[derive(Debug)]
pub enum LispError {
    LexerError(LexerError),
    ParserError(ParserError),
}

impl From<LexerError> for LispError {
    fn from(error: LexerError) -> Self {
        LispError::LexerError(error)
    }
}

impl From<ParserError> for LispError {
    fn from(error: ParserError) -> Self {
        LispError::ParserError(error)
    }
}

pub struct Parser<'a> {
    input: Peekable<Lexer<'a>>,
    end: Position,
    source: Option<String>
}

impl<'a> Parser<'a> {
    pub fn from_string(string: &'a String, source: Option<String>) -> Self {
        let lexer = Lexer::from_string(string, source.clone());
        Parser {
            input: lexer.peekable(),
            end: Position(0, 0),
            source
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>, LispError> {
        if let Some(lit_err) = self.input.next() {
            let lit = lit_err?;
            self.end = lit.end.clone();
            Ok(Some(lit))
        } else {
            Ok(None)
        }
    }

    pub fn next_expression(&mut self) -> Result<Option<Expression>, LispError> {
        if let Some(t) = self.next()? {
            match t.literal {
                Literal::Bool(v) => Ok(Some(Expression::Bool(v))),
                Literal::Char(v) => Ok(Some(Expression::Char(v))),
                Literal::String(v) => Ok(Some(Expression::String(v))),
                Literal::Identifier(v) => Ok(Some(Expression::Symbol(v))),
                Literal::Number(sign, base, body) => {
                    match isize::from_str_radix(&body.replace("_", ""), base as u32) {
                        Ok(i) => {
                            let number = if sign { i } else { -i };
                            Ok(Some(Expression::Integer(number)))
                        }
                        Err(_err) => {
                            if base == 10 {
                                let num = body.parse::<f64>();
                                match num {
                                    Ok(i) => {
                                        let number = if sign { i } else { -i };
                                        Ok(Some(Expression::Float(number)))
                                    }
                                    Err(_err) => Err(ParserError {
                                        start: t.start.clone(),
                                        end: self.end.clone(),
                                        source: self.source.clone(),
                                        error: InvalidNumberLiteral,
                                    })?,
                                }
                            } else {
                                Err(ParserError {
                                    start: t.start.clone(),
                                    end: self.end.clone(),
                                    source: self.source.clone(),
                                    error: InvalidNumberLiteral,
                                })?
                            }
                        }
                    }
                }
                Literal::LRoundBracket => Ok(Some(self.process_list(
                    t.start.clone(),
                    Literal::RRoundBracket,
                    false,
                )?)),
                Literal::LSquareBracket => Ok(Some(self.process_list(
                    t.start.clone(),
                    Literal::RSquareBracket,
                    false,
                )?)),
                Literal::HashLRoundBracket => Ok(Some(self.process_list(
                    t.start.clone(),
                    Literal::RRoundBracket,
                    true,
                )?)),
                Literal::HashLSquareBracket => Ok(Some(self.process_list(
                    t.start.clone(),
                    Literal::RSquareBracket,
                    true,
                )?)),
                Literal::AmpersandLRoundBracket => {
                    let body = self.process_simple_list(t.start.clone(), Literal::RRoundBracket)?;
                    Ok(Some(self.convert_hole_lambda_to_lambda(body)))
                }
                Literal::AmpersandLSquareBracket => {
                    let body =
                        self.process_simple_list(t.start.clone(), Literal::RSquareBracket)?;
                    Ok(Some(self.convert_hole_lambda_to_lambda(body)))
                }
                Literal::LCurlyBracket => {
                    let body = self.process_simple_list(t.start.clone(), Literal::RCurlyBracket)?;
                    match self.convert_infix_to_prefix(body) {
                        Ok(res) => Ok(Some(res)),
                        Err(error) => Err(ParserError {
                            start: t.start,
                            end: t.end,
                            source: self.source.clone(),
                            error: error,
                        })?,
                    }
                }
                Literal::RRoundBracket => Err(ParserError {
                    start: t.start,
                    end: t.end,
                    source: self.source.clone(),
                    error: UnbalancedBracket,
                })?,
                Literal::RSquareBracket => Err(ParserError {
                    start: t.start,
                    end: t.end,
                    source: self.source.clone(),
                    error: UnbalancedBracket,
                })?,
                Literal::RCurlyBracket => Err(ParserError {
                    start: t.start,
                    end: t.end,
                    source: self.source.clone(),
                    error: UnbalancedBracket,
                })?,
                Literal::Quote => {
                    match self.next_expression()? {
                        Some(d) => {
                            match d {
                                // TODO: Fix this, using next_expression here is not good
                                // because it uses `make_pair` and that returns Nil for empty pairs
                                Expression::Nil => Ok(Some(Expression::Nil)),
                                other => Ok(Some(Expression::List(vec![
                                    self.make_symbol("quote"),
                                    other,
                                ]))),
                            }
                        }
                        None => Err(ParserError {
                            start: t.start.clone(),
                            end: self.end.clone(),
                            source: self.source.clone(),
                            error: UnexpectedEndOfInput,
                        })?,
                    }
                }
                Literal::Quasiquote => match self.next_expression()? {
                    Some(d) => Ok(Some(Expression::List(vec![
                        self.make_symbol("quasiquote"),
                        d,
                    ]))),
                    None => Err(ParserError {
                        start: t.start.clone(),
                        end: self.end.clone(),
                        source: self.source.clone(),
                        error: UnexpectedEndOfInput,
                    })?,
                },
                Literal::Unquote => match self.next_expression()? {
                    Some(d) => Ok(Some(Expression::List(vec![self.make_symbol("unquote"), d]))),
                    None => Err(ParserError {
                        start: t.start.clone(),
                        end: self.end.clone(),
                        source: self.source.clone(),
                        error: UnexpectedEndOfInput,
                    })?,
                },
                Literal::UnquoteSplicing => match self.next_expression()? {
                    Some(d) => Ok(Some(Expression::List(vec![
                        self.make_symbol("unquote-splicing"),
                        d,
                    ]))),
                    None => Err(ParserError {
                        start: t.start.clone(),
                        end: self.end.clone(),
                        source: self.source.clone(),
                        error: UnexpectedEndOfInput,
                    })?,
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
    ) -> Result<Vec<Expression>, LispError> {
        let mut res = Vec::new();

        loop {
            if self.is_peek_none() {
                return Err(ParserError {
                    start: start.clone(),
                    end: self.end.clone(),
                    source: self.source.clone(),
                    error: UnexpectedEndOfInput,
                })?;
            }

            if self.is_peek_eq(&closing)? {
                self.next()?;
                break;
            } else if self.is_peek_eq(&Literal::Dot)? {
                return Err(ParserError {
                    start: start.clone(),
                    end: self.end.clone(),
                    source: self.source.clone(),
                    error: UnexpectedDot,
                })?;
            } else {
                if let Some(n) = self.next_expression()? {
                    res.push(n);
                } else {
                    panic!("Unexpected end of input")
                }
            }
        }

        Ok(res)
    }

    fn process_list(
        &mut self,
        start: Position,
        closing: Literal,
        is_vector: bool,
    ) -> Result<Expression, LispError> {
        let mut res = Vec::new();

        loop {
            if self.is_peek_none() {
                return Err(ParserError {
                    start: start.clone(),
                    end: self.end.clone(),
                    source: self.source.clone(),
                    error: UnexpectedEndOfInput,
                })?;
            }

            if self.is_peek_eq(&closing)? {
                self.next()?;
                break;
            } else if self.is_peek_eq(&Literal::Dot)? {
                if is_vector {
                    return Err(ParserError {
                        start: start.clone(),
                        end: self.end.clone(),
                        source: self.source.clone(),
                        error: UnexpectedDot,
                    })?;
                }
                // skip dot
                self.next()?;

                let tail = match self.next_expression()? {
                    Some(d) => d,
                    None => {
                        return Err(ParserError {
                            start: start.clone(),
                            end: self.end.clone(),
                            source: self.source.clone(),
                            error: InvalidDottedList,
                        })?
                    }
                };

                if self.is_peek_eq(&closing)? {
                    self.next()?;
                } else {
                    return Err(ParserError {
                        start: start.clone(),
                        end: self.end.clone(),
                        source: self.source.clone(),
                        error: InvalidDottedList,
                    })?;
                }

                return Ok(Expression::DottedList(res, Box::new(tail)));
            } else {
                if let Some(n) = self.next_expression()? {
                    res.push(n);
                } else {
                    panic!("Unexpected end of input")
                }
            }
        }

        if is_vector {
            Ok(Expression::Vector(res))
        } else {
            if res.len() == 0 {
                Ok(Expression::Nil)
            } else {
                Ok(Expression::List(res))
            }
        }
    }

    fn make_symbol(&mut self, sym: &str) -> Expression {
        Expression::Symbol(String::from(sym))
    }

    fn find_max_hole(&mut self, datum: &Expression) -> isize {
        let mut max = 0;
        match datum {
            &Expression::List(ref elems) => {
                for d in elems {
                    let res = self.find_max_hole(&d);
                    if res > max {
                        max = res;
                    }
                }
            }
            &Expression::Symbol(ref id) => {
                let mut tmp = id.clone();
                let first = tmp.remove(0);

                if first == '&' {
                    let res = tmp.parse::<isize>().expect("Could not parse hole index");
                    if res > max {
                        max = res
                    }
                }
            }
            _ => (),
        }
        max
    }

    fn convert_hole_lambda_to_lambda(&mut self, datums: Vec<Expression>) -> Expression {
        let body = Expression::List(datums);
        let max = self.find_max_hole(&body);

        let mut params = Vec::new();

        for i in 1..(max + 1) {
            let param = format!("&{}", i);
            params.push(self.make_symbol(&param));
        }

        Expression::List(vec![self.make_symbol("fn"), Expression::List(params), body])
    }

    // Converts a list of the form {1 + 2 + 3} to (+ 1 2 3)
    fn convert_infix_to_prefix(
        &mut self,
        datums: Vec<Expression>,
    ) -> Result<Expression, ParserErrorType> {
        // Infix lists must have an odd number of elements
        // and at least 3
        if datums.len() < 3 || (datums.len() % 2 == 0) {
            return Err(InvalidInfixList);
        }

        let op = datums.get(1).unwrap().clone();
        let mut args = vec![
            op.clone(),
            datums.get(0).unwrap().clone(),
            datums.get(2).unwrap().clone(),
        ];

        for i in 3..datums.len() {
            if i % 2 == 0 {
                args.push(datums.get(i).unwrap().clone());
            } else {
                if datums.get(i).unwrap() != &op {
                    return Err(InvalidInfixList);
                }
            }
        }

        Ok(Expression::List(args))
    }
}
