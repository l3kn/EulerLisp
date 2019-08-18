use std::iter::Peekable;
use std::result::Result;
use std::str::Chars;

use crate::LispError;

#[derive(Debug, Clone)]
pub struct LexerError {
    start: Position,
    end: Position,
    error: LexerErrorType,
}

#[derive(Debug, Clone)]
pub enum LexerErrorType {
    UnexpectedEndOfInput,
    InvalidNamedCharLiteral,
    InvalidIdentifier,
    UnknownToken,
    // "got char, expected one of Vec<char>"
    UnexpectedCharacter(char, Vec<char>),
    InvalidStringEscape(char),
}

use self::LexerErrorType::*;

impl From<LexerError> for LispError {
    fn from(error: LexerError) -> Self {
        LispError::LexerError(error)
    }
}

pub struct Lexer<'a> {
    line: usize,
    column: usize,
    input: Peekable<Chars<'a>>,
    source: Option<String>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position(pub usize, pub usize);

#[derive(Debug, PartialEq)]
pub struct Token {
    pub start: Position,
    pub end: Position,
    pub literal: Literal,
    pub source: Option<String>,
}

impl Token {
    pub fn new(start: Position, end: Position, literal: Literal, source: Option<String>) -> Self {
        Self {
            start,
            end,
            literal,
            source,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Bool(bool),
    Char(char),
    String(String),
    Identifier(String),
    Comment(String),
    // (sign, base, body, sign_given?, base_given?),
    // will be converted to the correct number type by the parser
    //
    // `sign_given?` and `base_given?`
    // are used to tell `10` from `#d+10`
    // when outputting in the code formatter
    Number(bool, usize, String, bool, bool),
    LRoundBracket,
    LSquareBracket,
    LCurlyBracket,
    HashLRoundBracket,
    HashLSquareBracket,
    AmpersandLRoundBracket,
    AmpersandLSquareBracket,
    RRoundBracket,
    RSquareBracket,
    RCurlyBracket,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    Dot,
}

impl Literal {
    pub fn to_string(&self) -> String {
        match *self {
            Literal::Bool(true) => String::from("#t"),
            Literal::Bool(false) => String::from("#f"),
            Literal::Char(' ') => String::from("#\\space"),
            Literal::Char('\n') => String::from("#\\newline"),
            Literal::Char(c) => format!("#\\{}", c),
            Literal::String(ref s) => {
                let mut res = s.replace("\n", "\\n");
                res = res.replace("\r", "\\r");
                res = res.replace("\t", "\\t");
                res = res.replace("\"", "\\\"");
                res = res.replace("\\", "\\\\");

                format!("\"{}\"", res)
            }
            Literal::Identifier(ref s) => s.clone(),
            // TODO: Check if `body` contains `_`
            // and reformat it so that digits are separated into groups of three
            Literal::Number(sign, base, ref body, sign_given, base_given) => {
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
                string
            }
            Literal::Comment(ref s) => format!(";{}", s),
            Literal::LRoundBracket => String::from("("),
            Literal::LSquareBracket => String::from("["),
            Literal::LCurlyBracket => String::from("{"),
            Literal::HashLRoundBracket => String::from("#("),
            Literal::HashLSquareBracket => String::from("#["),
            Literal::AmpersandLRoundBracket => String::from("&("),
            Literal::AmpersandLSquareBracket => String::from("&["),
            Literal::RRoundBracket => String::from(")"),
            Literal::RSquareBracket => String::from("]"),
            Literal::RCurlyBracket => String::from("}"),
            Literal::Quote => String::from("'"),
            Literal::Quasiquote => String::from("`"),
            Literal::Unquote => String::from(","),
            Literal::UnquoteSplicing => String::from(",@"),
            Literal::Dot => String::from("."),
        }
    }
}

// TODO:
//
// This just flips the type of Iterator::next_token()
// from Result<Option<Token>, LexerError>
// to Option<Result<Token, LexerError>>
//
// It would be more elegant to change next_token() itself
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(maybe_token) => {
                if let Some(token) = maybe_token {
                    Some(Ok(token))
                } else {
                    None
                }
            }
            Err(err) => Some(Err(err)),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn from_string(string: &'a str, source: Option<String>) -> Self {
        let input = string.chars().peekable();
        // colum needs to start at 0 because it is incremented
        // each time .next() is called
        Lexer {
            line: 1,
            column: 0,
            input,
            source,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn is_peek_eq(&mut self, c: char) -> bool {
        Some(&c) == self.input.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.column += 1;
        self.input.next()
    }

    // Get the next char and skip over any whitespace
    // or comments `;...<end of line>`
    fn next_skipping_whitespace(&mut self) -> Option<char> {
        loop {
            match self.next() {
                Some('\t') => (),
                Some(' ') => (),
                Some('\n') => {
                    self.column = 1;
                    self.line += 1;
                }
                other => return other,
            }
        }
    }

    fn is_peek_delimiter(&mut self) -> bool {
        if let Some(p) = self.peek() {
            match *p {
                '(' | '[' | '{' | ')' | ']' | '}' => true,
                ' ' | '\t' | '\n' => true,
                '"' => true,
                ';' => true,
                _ => false,
            }
        } else {
            true
        }
    }

    // Subsequent chars of an identifier can be
    //   1. letters: a...z
    //   2. digits: 0...9
    //   3. special subsequents: + - @
    //   4. initials: ! $ % & * / : < = > ? ^ _ ~
    fn is_peek_subsequent(&mut self) -> bool {
        if let Some(p) = self.peek() {
            match *p {
                'A'...'Z' => true,
                'a'...'z' => true,
                '0'...'9' => true,
                '+' | '-' | '.' => true,
                '@' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^'
                | '_' | '~' => true,
                _ => false,
            }
        } else {
            true
        }
    }

    fn is_peek_digit(&mut self) -> bool {
        if let Some(p) = self.peek() {
            match *p {
                '0'...'9' => true,
                _ => false,
            }
        } else {
            false
        }
    }

    fn read_to_delimiter(&mut self) -> String {
        let mut res = Vec::new();
        while !self.is_peek_delimiter() {
            // This should never happen,
            // if input.peek() was None,
            // is_peek_delimiter would have returned true
            let n = self.next().expect("Unexpected end of input");
            res.push(n);
        }
        let mut body = String::new();
        body.extend(res.iter());
        body
    }

    fn pos(&self) -> Position {
        Position(self.line, self.column)
    }

    fn make_token(&self, start: Position, literal: Literal) -> Result<Token, LexerError> {
        Ok(Token::new(start, self.pos(), literal, self.source.clone()))
    }

    fn make_error(&self, start: Position, error: LexerErrorType) -> Result<Token, LexerError> {
        Err(LexerError {
            start,
            end: self.pos(),
            error,
        })
    }

    // true -> positive
    // false -> negative
    // (sign, sign_given?)
    fn read_optional_sign(&mut self) -> (bool, bool) {
        if self.is_peek_eq('-') {
            self.next();
            (false, true)
        } else if self.is_peek_eq('+') {
            self.next();
            (true, true)
        } else {
            (true, false)
        }
    }

    fn process_identifier(&mut self, start: Position, first: char) -> Result<Token, LexerError> {
        let mut name = String::new();
        name.push(first);

        while !self.is_peek_delimiter() {
            if self.is_peek_subsequent() {
                if let Some(c) = self.next() {
                    name.push(c);
                } else {
                    panic!("Unexpected end of input");
                }
            } else {
                return Err(LexerError {
                    start,
                    end: self.pos(),
                    error: InvalidIdentifier,
                });
            }
        }

        self.make_token(start, Literal::Identifier(name))
    }

    // Tokens can have the forms
    // * <identifier>
    // * <boolean>
    //   * #t
    //   * #f
    // * <number> = <prefix?><sign?><body> (simplified, no support for rationals etc)
    //     * #b, #o, #d, #f
    //     * +, -
    //     * <alphanum>*
    // * <character>
    // * <string>
    // * brackets: '(' '[' '#[' '#(' ']' ')'
    // * quotes: '\'' '`' ',' ',@' (quote, quasiquote, unquote, unquote-splicing)
    // * '.'
    //
    // '.' is only used inside dotted lists (cons)
    // #( and #[ are used for vector literals
    pub fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        let maybe_next = self.next_skipping_whitespace();
        if let Some(next) = maybe_next {
            let start = self.pos();
            let token: Result<Token, LexerError> = match next {
                ';' => {
                    let mut text = String::new();
                    loop {
                        match self.next() {
                            Some('\n') => {
                                self.line += 1;
                                break;
                            }
                            Some(c) => text.push(c),
                            None => break,
                        }
                    }
                    self.make_token(start, Literal::Comment(text))
                }
                '#' => match self.next() {
                    Some('t') => self.make_token(start, Literal::Bool(true)),
                    Some('f') => self.make_token(start, Literal::Bool(false)),
                    Some('b') => {
                        let (sign, sign_given) = self.read_optional_sign();
                        let body = self.read_to_delimiter();
                        self.make_token(start, Literal::Number(sign, 2, body, sign_given, true))
                    }
                    Some('o') => {
                        let (sign, sign_given) = self.read_optional_sign();
                        let body = self.read_to_delimiter();
                        self.make_token(start, Literal::Number(sign, 8, body, sign_given, true))
                    }
                    Some('d') => {
                        let (sign, sign_given) = self.read_optional_sign();
                        let body = self.read_to_delimiter();
                        self.make_token(start, Literal::Number(sign, 10, body, sign_given, true))
                    }
                    Some('x') => {
                        let (sign, sign_given) = self.read_optional_sign();
                        let body = self.read_to_delimiter();
                        self.make_token(start, Literal::Number(sign, 16, body, sign_given, true))
                    }
                    Some('\\') => self.process_char_literal(start),
                    Some('(') => self.make_token(start, Literal::HashLRoundBracket),
                    Some('[') => self.make_token(start, Literal::HashLSquareBracket),
                    Some(other) => self.make_error(
                        start,
                        UnexpectedCharacter(
                            other,
                            vec!['t', 'f', '/', '(', '[', 'b', 'o', 'd', 'x'],
                        ),
                    ),
                    None => self.make_error(start, UnexpectedEndOfInput),
                },
                '"' => {
                    let mut res = String::new();
                    loop {
                        match self.next() {
                            Some('"') => break self.make_token(start, Literal::String(res)),
                            Some('\\') => match self.next() {
                                Some('n') => res.push('\n'),
                                Some('r') => res.push('\r'),
                                Some('t') => res.push('\t'),
                                Some('"') => res.push('"'),
                                Some('\\') => res.push('\\'),
                                Some(other) => {
                                    break self.make_error(start, InvalidStringEscape(other));
                                }
                                None => break self.make_error(start, UnexpectedEndOfInput),
                            },
                            Some(other) => res.push(other),
                            None => break self.make_error(start, UnexpectedEndOfInput),
                        }
                    }
                }
                '(' => self.make_token(start, Literal::LRoundBracket),
                '[' => self.make_token(start, Literal::LSquareBracket),
                '{' => self.make_token(start, Literal::LCurlyBracket),
                ')' => self.make_token(start, Literal::RRoundBracket),
                ']' => self.make_token(start, Literal::RSquareBracket),
                '}' => self.make_token(start, Literal::RCurlyBracket),
                // The only tokens that are allowed to begin with `.`
                // are `.` itself (the dot in dotted lists)
                // and the identifier `...`
                '.' => {
                    if self.is_peek_eq('.') {
                        let rest = self.read_to_delimiter();
                        if rest == ".." {
                            self.make_token(start, Literal::Identifier(String::from("...")))
                        } else {
                            Err(LexerError {
                                start,
                                end: self.pos(),
                                error: UnexpectedEndOfInput,
                            })
                        }
                    } else {
                        self.make_token(start, Literal::Dot)
                    }
                }
                '\'' => self.make_token(start, Literal::Quote),
                '`' => self.make_token(start, Literal::Quasiquote),
                ',' => {
                    if self.is_peek_eq('@') {
                        self.next();
                        self.make_token(start, Literal::UnquoteSplicing)
                    } else {
                        self.make_token(start, Literal::Unquote)
                    }
                }
                // `+` is only allowed as a identifier
                // or as the sign of a number
                '+' => {
                    if self.is_peek_delimiter() {
                        self.make_token(start, Literal::Identifier(String::from("+")))
                    } else if self.is_peek_digit() {
                        let body = self.read_to_delimiter();
                        self.make_token(start, Literal::Number(true, 10, body, true, false))
                    } else {
                        self.make_error(start, InvalidIdentifier)
                    }
                }
                // `-` is only allowed as a identifier
                // or as the sign of a number
                '-' => {
                    if self.is_peek_delimiter() {
                        self.make_token(start, Literal::Identifier(String::from("-")))
                    } else if self.is_peek_digit() {
                        let body = self.read_to_delimiter();
                        self.make_token(start, Literal::Number(false, 10, body, true, false))
                    } else {
                        self.make_error(start, InvalidIdentifier)
                    }
                }
                // In addition to the syntax described in R5RS,
                // there is a short form for lambdas
                // &(+ &1 &2) => (fn (a b) (+ a b))
                '&' => {
                    if self.is_peek_eq('(') {
                        self.next();
                        self.make_token(start, Literal::AmpersandLRoundBracket)
                    } else if self.is_peek_eq('[') {
                        self.next();
                        self.make_token(start, Literal::AmpersandLSquareBracket)
                    } else {
                        self.process_identifier(start, '&')
                    }
                }
                first @ 'A'...'Z'
                | first @ 'a'...'z'
                | first @ '!'
                | first @ '$'
                | first @ '%'
                | first @ '*'
                | first @ '/'
                | first @ ':'
                | first @ '<'
                | first @ '='
                | first @ '>'
                | first @ '?'
                | first @ '^'
                | first @ '_'
                | first @ '~' => self.process_identifier(start, first),
                first @ '0'...'9' => {
                    let rest = self.read_to_delimiter();

                    let mut body = String::new();
                    body.push(first);
                    body += &rest;

                    self.make_token(start, Literal::Number(true, 10, body, false, false))
                }
                _ => self.make_error(start, UnknownToken),
            };
            Ok(Some(token?))
        } else {
            Ok(None)
        }
    }

    // Char literals can have the forms
    // * #\<char>
    // * #\space
    // * #\newline
    fn process_char_literal(&mut self, start: Position) -> Result<Token, LexerError> {
        let next = self.next();
        if let Some(c) = next {
            match c {
                's' => {
                    if self.is_peek_delimiter() {
                        self.make_token(start, Literal::Char('s'))
                    } else {
                        for e in ['p', 'a', 'c', 'e'].iter() {
                            if self.next() != Some(*e) {
                                return self.make_error(start, InvalidNamedCharLiteral);
                            }
                        }
                        self.make_token(start, Literal::Char(' '))
                    }
                }
                'n' => {
                    if self.is_peek_delimiter() {
                        self.make_token(start, Literal::Char('n'))
                    } else {
                        for e in ['e', 'w', 'l', 'i', 'n', 'e'].iter() {
                            if self.next() != Some(*e) {
                                return self.make_error(start, InvalidNamedCharLiteral);
                            }
                        }
                        self.make_token(start, Literal::Char('\n'))
                    }
                }
                other => self.make_token(start, Literal::Char(other)),
            }
        } else {
            self.make_error(start, UnexpectedEndOfInput)
        }
    }
}
