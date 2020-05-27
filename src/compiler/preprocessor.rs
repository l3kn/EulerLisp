use std::collections::HashMap;
use std::rc::Rc;

use crate::env::{AEnv, AEnvRef};
use crate::symbol_table::{self, Symbol};
use crate::syntax_rule::SyntaxRule;
use crate::vm::Context;
use crate::{LispResult, Value};

pub use super::constant_folding;
pub use super::error::Error;

#[derive(Debug)]
pub enum Meaning {
    Quotation(Value),
    // arity, body, dotted?, macro?
    Abstraction(usize, Box<Meaning>, bool, bool),
    Alternative(Box<Meaning>, Box<Meaning>, Box<Meaning>),
    Sequence(Vec<Meaning>),
    Assignment(Reference, Box<Meaning>),
    // MacroAssignment(Reference, Box<Meaning>),
    Reference(Reference),
    BuiltinApplication(Symbol, Vec<Meaning>),
    ClosedApplicationNil(Box<Meaning>, bool),
    ClosedApplication(Vec<Meaning>, Box<Meaning>, bool),
    // Function, Arguments, Tail?
    RegularApplication(Box<Meaning>, Vec<Meaning>, bool),
}

#[derive(Debug)]
pub enum Reference {
    Local(usize, usize),
    Global(usize),
    Constant(usize),
}

pub struct Preprocessor {
    pub syntax_rules: HashMap<Symbol, SyntaxRule>,
    context: Rc<Context>,
}

impl Preprocessor {
    pub fn new(context: Rc<Context>) -> Self {
        Self {
            context,
            syntax_rules: HashMap::new(),
        }
    }

    pub fn is_reserved(&self, symbol: Symbol) -> bool {
        // NOTE: This assumes a fixed ordering of reserved symbols
        symbol < symbol_table::LET
    }

    pub fn preprocess(&mut self, datums: Vec<Value>, tail: bool) -> LispResult<Vec<Meaning>> {
        let mut extracted_datums: Vec<Value> = vec![];
        for datum in datums.into_iter() {
            if let Some(v) = self.extract_macros(datum)? {
                extracted_datums.push(v);
            }
        }

        let mut expanded_datums: Vec<Value> = vec![];
        for datum in extracted_datums.into_iter() {
            let v = self.expand_macros(datum)?;
            expanded_datums.push(v);
        }

        let mut extracted_datums2: Vec<Value> = vec![];
        for datum in expanded_datums.into_iter() {
            if let Some(v) = self.extract_constants(datum)? {
                extracted_datums2.push(v);
            }
        }

        let datums: LispResult<Vec<Value>> = extracted_datums2
            .into_iter()
            .map(|v| self.convert_outer_defs(v))
            .collect();
        let datums: LispResult<Vec<Value>> = datums?
            .into_iter()
            .map(|v| self.convert_inner_defs(v))
            .collect();
        let datums: Vec<Value> = datums?.into_iter().map(constant_folding::fold).collect();

        let meanings = self.preprocess_meanings(datums.clone(), tail)?;
        Ok(meanings)
    }

    pub fn extract_constants(&mut self, datum: Value) -> LispResult<Option<Value>> {
        if datum.is_true_list() {
            let mut elems = datum.as_list()?;
            let name = elems.remove(0);
            if let Value::Symbol(s) = name {
                if s == symbol_table::DEFCONST {
                    let name_sym = elems.remove(0).as_symbol()?;
                    let value = constant_folding::fold(elems.remove(0));

                    if self.is_reserved(name_sym) {
                        Err(Error::ReservedName(name_sym))?;
                    }

                    if !value.is_self_evaluating() {
                        Err(Error::NonSelfEvaluatingConstant(name_sym))?;
                    }

                    self.context.add_constant(name_sym, value);
                    return Ok(None);
                }
            }
        }

        Ok(Some(datum))
    }

    pub fn extract_macros(&mut self, datum: Value) -> LispResult<Option<Value>> {
        if datum.is_true_list() {
            let mut elems = datum.as_list()?;
            let name = elems.remove(0);
            if let Value::Symbol(s) = name {
                if s == symbol_table::DEFSYNTAX {
                    let rule_sym = elems.remove(0).as_symbol()?;
                    let literals = elems.remove(0).as_list()?;
                    let rules = elems.remove(0).as_list()?;
                    let syntax_rule = SyntaxRule::parse(rule_sym, literals, rules)?;

                    if self.is_reserved(rule_sym) {
                        Err(Error::ReservedName(rule_sym))?;
                    }
                    self.syntax_rules.insert(rule_sym, syntax_rule);
                    return Ok(None);
                }
            }
        }

        Ok(Some(datum))
    }

    pub fn expand_macros(&mut self, datum: Value) -> LispResult<Value> {
        if datum.is_true_list() {
            let mut elems = datum.as_list()?;
            // FIXME: A list should never be empty,
            // how does this happen?
            if elems.is_empty() {
                return Ok(Value::Nil);
            }
            // Wrong order
            let name = elems[0].clone();
            if let Value::Symbol(sym) = name {
                // FIXME: Do this without the clone
                let rules = self.syntax_rules.clone();
                let rule = rules.get(&sym);
                if rule.is_none() {
                    let elems: LispResult<Vec<Value>> =
                        elems.into_iter().map(|d| self.expand_macros(d)).collect();
                    return Ok(Value::make_list_from_vec(elems?));
                }
                let sr = rule?.clone();
                elems.remove(0);
                if let Some(ex) = sr.apply(elems.clone())? {
                    self.expand_macros(ex)
                } else {
                    Err(Error::NoMatchingMacroPattern(Value::make_list_from_vec(
                        elems,
                    )))?
                }
            } else {
                let elems: LispResult<Vec<Value>> =
                    elems.into_iter().map(|d| self.expand_macros(d)).collect();
                return Ok(Value::make_list_from_vec(elems?));
            }
        } else {
            Ok(datum)
        }
    }

    pub fn convert_outer_defs(&mut self, datum: Value) -> LispResult<Value> {
        if datum.is_true_list() {
            let mut elems = datum.as_list()?;
            let name_sym = elems.remove(0);
            if let Value::Symbol(s) = name_sym {
                if s == symbol_table::DEF {
                    let var_sym = elems.remove(0).as_symbol()?;
                    let value = elems.remove(0);

                    if self.is_reserved(var_sym) {
                        Err(Error::ReservedName(var_sym))?;
                    }

                    if self.context.lookup_global_index(var_sym).is_none() {
                        self.context.add_global(var_sym, Value::Undefined);
                    }

                    return Ok(Value::make_list_from_vec(vec![
                        Value::Symbol(symbol_table::SET),
                        Value::Symbol(var_sym),
                        value,
                    ]));
                }
                // else if s == symbol_table::DEFMACRO {
                //     let var_sym = elems.remove(0).as_symbol()?;
                //     let value = elems.remove(0);

                //     if self.is_reserved(var_sym) {
                //         Err(Error::ReservedName(var_sym))?;
                //     }

                //     if self.context.lookup_global_index(var_sym).is_none() {
                //         self.context.add_global(var_sym, Value::Undefined);
                //     }

                //     return Ok(Value::make_list_from_vec(vec![
                //         Value::Symbol(symbol_table::SETMACRO),
                //         Value::Symbol(var_sym),
                //         value,
                //     ]));
                // }
            }
        }

        Ok(datum)
    }

    // Convert function bodies with internal definitions
    // (only allowed at the top)
    // to a equivalent let(rec) expression
    //
    // ``` scheme
    // (def foo (fn (a)
    //   (def n 100)
    //   (def bar (fn (b) (+ b n)))
    //   (bar a)))
    // ```
    //
    // is converted to
    //
    // ``` scheme
    // (def foo (fn (a)
    //   (let ((n #undefined)
    //         (bar #undefined))
    //      (set! n 100)
    //      (set! bar (fn (b) (+ b n)))
    //      (bar a))))
    // ```
    pub fn convert_inner_defs(&mut self, datum: Value) -> LispResult<Value> {
        if datum.is_true_list() {
            let elems = datum.as_list()?;
            let res: LispResult<Vec<Value>> = elems
                .into_iter()
                .map(|d| self.convert_inner_defs(d))
                .collect();

            let mut elems = res?;
            if let Value::Symbol(s) = elems[0].clone() {
                if s == symbol_table::FN {
                    elems.remove(0);
                    let args = elems.remove(0);

                    let mut defs = Vec::new();
                    let mut bodies = Vec::new();
                    let mut found_non_def = false;

                    for body in &elems {
                        if body.is_true_list() {
                            let mut b_elems = body.as_list()?;
                            let b_name = b_elems.remove(0);
                            if let Value::Symbol(sym) = b_name {
                                if sym == symbol_table::DEF {
                                    if found_non_def {
                                        Err(Error::InvalidInternalDefinition)?;
                                    }
                                    let def_name = b_elems.remove(0);
                                    let def_value = b_elems.remove(0);

                                    defs.push((def_name, def_value));
                                } else {
                                    found_non_def = true;
                                    bodies.push(body.clone());
                                }
                            } else {
                                found_non_def = true;
                                bodies.push(body.clone());
                            }
                        } else {
                            bodies.push(body.clone())
                        }
                    }

                    if defs.is_empty() {
                        let mut fn_ = vec![Value::Symbol(symbol_table::FN), args];
                        fn_.extend(bodies);
                        return Ok(Value::make_list_from_vec(fn_));
                    }

                    let mut let_bindings = Vec::new();
                    for &(ref n, ref _v) in &defs {
                        let datum_ = vec![n.clone(), Value::Undefined];
                        let_bindings.push(Value::make_list_from_vec(datum_))
                    }

                    let mut let_ = vec![
                        Value::Symbol(symbol_table::LET),
                        Value::make_list_from_vec(let_bindings),
                    ];

                    for (n, v) in defs {
                        let datum_ = vec![Value::Symbol(symbol_table::SET), n, v];
                        let_.push(Value::make_list_from_vec(datum_));
                    }
                    let_.extend(bodies);

                    let fn_ = vec![
                        Value::Symbol(symbol_table::FN),
                        args,
                        Value::make_list_from_vec(let_),
                    ];

                    let res = Value::make_list_from_vec(fn_);
                    return self.expand_macros(res);
                }
            }
            return Ok(Value::make_list_from_vec(elems));
        }
        Ok(datum)
    }

    pub fn preprocess_meanings(
        &mut self,
        datums: Vec<Value>,
        tail: bool,
    ) -> LispResult<Vec<Meaning>> {
        let mut meanings = vec![];
        if datums.len() > 0 {
            let last = datums.len() - 1;
            for (i, d) in datums.into_iter().enumerate() {
                // Why construct a new aenv each time?
                // Because it's only used one time?
                let empty_aenv = AEnv::new(None);
                let aenv_ref = Rc::new(empty_aenv);
                let meaning = self.preprocess_meaning(d, aenv_ref, tail && i == last)?;
                meanings.push(meaning);
            }
        }
        return Ok(meanings);
    }

    pub fn preprocess_meaning(
        &mut self,
        datum: Value,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Meaning> {
        match datum {
            Value::Pair(ref elems_) if datum.is_true_list() => {
                let mut elems = elems_.collect_list()?;
                let name_sym = elems.remove(0);

                if let Value::Symbol(sym) = name_sym {
                    if sym == symbol_table::QUOTE {
                        self.preprocess_meaning_quotation(elems.remove(0), env, tail)
                    } else if sym == symbol_table::FN {
                        self.preprocess_meaning_abstraction(elems, env, false, tail)
                    } else if sym == symbol_table::MACRO {
                        self.preprocess_meaning_abstraction(elems, env, true, tail)
                    } else if sym == symbol_table::IF {
                        self.preprocess_meaning_alternative(elems, env, tail)
                    } else if sym == symbol_table::DO {
                        self.preprocess_meaning_sequence(elems, env, tail)
                    } else if sym == symbol_table::SET {
                        self.preprocess_meaning_assignment(elems, env, tail)
                    // } else if sym == symbol_table::SETMACRO {
                    //     self.preprocess_meaning_macro_assignment(elems, env, tail)
                    } else {
                        // FIXME: Do this without the clone
                        let rules = self.syntax_rules.clone();
                        let rule = rules.get(&sym);
                        if rule.is_none() {
                            return self.preprocess_meaning_application(name_sym, elems, env, tail);
                        }
                        let sr = rule?.clone();
                        match sr.apply(elems.clone())? {
                            Some(ex) => self.preprocess_meaning(ex, env, tail),
                            None => {
                                return Err(Error::NoMatchingMacroPattern(
                                    Value::make_list_from_vec(elems),
                                ))?;
                            }
                        }
                    }
                } else {
                    self.preprocess_meaning_application(name_sym, elems, env, tail)
                }
            }
            Value::Symbol(symbol) => self.preprocess_meaning_reference(symbol, env, tail),
            other => self.preprocess_meaning_quotation(other, env, tail),
        }
    }

    fn reference_type(&self, symbol: Symbol, env: AEnvRef) -> LispResult<Reference> {
        if let Some(binding) = env.lookup(symbol) {
            return Ok(Reference::Local(binding.0, binding.1));
        }

        if let Some(index) = self.context.lookup_constant_index(symbol) {
            return Ok(Reference::Constant(index));
        }

        if let Some(index) = self.context.lookup_global_index(symbol) {
            return Ok(Reference::Global(index));
        } else {
            // TODO: Collect warnings in array, display in REPL / compiler
            println!(
                "; Warning: Reference to unbound global variable {}",
                symbol.string()
            );
            let index = self.context.add_global(symbol, Value::Undefined);
            Ok(Reference::Global(index))
        }
    }

    // TODO: Inline
    fn preprocess_meaning_reference(
        &mut self,
        symbol: Symbol,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Meaning> {
        Ok(Meaning::Reference(self.reference_type(symbol, env)?))
    }

    fn preprocess_meaning_assignment(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Meaning> {
        // TODO: Check arity
        let symbol = datums.remove(0).as_symbol()?;
        let mut res = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;

        let reference_type = self.reference_type(symbol, env)?;
        if let Reference::Constant(_i) = reference_type {
            Err(Error::ConstantReassignment(symbol))?
        } else {
            Ok(Meaning::Assignment(reference_type, Box::new(res)))
        }
    }

    fn preprocess_meaning_macro_assignment(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Meaning> {
        // TODO: Check arity
        let symbol = datums.remove(0).as_symbol()?;
        let mut res = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;

        let reference_type = self.reference_type(symbol, env)?;
        if let Reference::Constant(_i) = reference_type {
            Err(Error::ConstantReassignment(symbol))?
        } else {
            unimplemented!();
            // Ok(Meaning::MacroAssignment(reference_type, Box::new(res)))
        }
    }

    fn preprocess_meaning_quotation(
        &mut self,
        datum: Value,
        _env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Meaning> {
        Ok(Meaning::Quotation(datum))
    }

    fn preprocess_meaning_abstraction(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        is_macro: bool,
        tail: bool,
    ) -> LispResult<Meaning> {
        let names_ = datums.remove(0);
        let mut dotted = false;
        let mut names = Vec::new();

        let true_list = names_.is_true_list();
        match names_ {
            Value::Pair(_pair) => {
                if true_list {
                    let elems = _pair.collect_list()?;
                    for e in elems {
                        let sym = e.as_symbol()?;
                        names.push(sym);
                    }
                } else {
                    let elems = _pair.collect();
                    for e in elems {
                        let sym = e.as_symbol()?;
                        names.push(sym);
                    }
                    dotted = true
                }
            }
            Value::Symbol(sym) => {
                names.push(sym);
                dotted = true;
            }
            Value::Nil => {}
            // TODO: What about ((if :t car cdr) (cons 1 2))?
            _ => panic!("First argument to fn must be a list or a symbol"),
        }

        let arity = names.len();
        let mut env2 = AEnv::new(Some(env));
        env2.extend(names);

        let env2ref = Rc::new(env2);
        let body = self.preprocess_meaning_sequence(datums, env2ref, true)?;

        Ok(Meaning::Abstraction(
            arity,
            Box::new(body),
            dotted,
            is_macro,
        ))
    }

    fn preprocess_meaning_alternative(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Meaning> {
        let mut test = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;
        let mut cons = self.preprocess_meaning(datums.remove(0), env.clone(), tail)?;

        let mut alt = if datums.is_empty() {
            let c = self.context.add_anonymous_constant(Value::Nil);
            Meaning::Reference(Reference::Constant(c))
        } else {
            self.preprocess_meaning(datums.remove(0), env, tail)?
        };

        Ok(Meaning::Alternative(
            Box::new(test),
            Box::new(cons),
            Box::new(alt),
        ))
    }

    fn preprocess_meaning_sequence(
        &mut self,
        datums: Vec<Value>,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Meaning> {
        let mut res = Vec::new();

        if !datums.is_empty() {
            let last = datums.len() - 1;
            for (i, d) in datums.into_iter().enumerate() {
                let m = self.preprocess_meaning(d, env.clone(), (i == last) && tail)?;
                res.push(m);
            }
        }

        Ok(Meaning::Sequence(res))
    }

    /// Applications can have three forms:
    /// - builtin (* 1 2)
    /// - closed ((fn (k) (+ k 1)) 2)
    /// - regular (user-defined 1 2 3)
    fn preprocess_meaning_application(
        &mut self,
        fun: Value,
        datums: Vec<Value>,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Meaning> {
        use symbol_table::*;

        let mut args: Vec<Meaning> = Vec::new();

        for d in datums {
            let res = self.preprocess_meaning(d, env.clone(), false)?;
            args.push(res);
        }

        // TODO: At some point it would be nice to allow predifined variables, too.
        // Once this is implemented, there needs to be a check here.
        let arity = args.len();

        if let Value::Symbol(name) = fun {
            match name {
                INC | DEC | FST | RST | NOT | IS_ZERO | IS_NIL | CALL_CC | EVAL | BIN_EQ
                | BIN_EQUAL | BIN_LT | BIN_LTE | BIN_GT | BIN_GTE | NE | CONS | MOD
                | VECTOR_REF | APPLY | ADD | SUB | MUL | DIV | IDIV | VECTOR_SET | EVAL => {
                    return Ok(Meaning::BuiltinApplication(name, args));
                }
                _ => {}
            }
        }

        if fun.is_true_list() {
            let funl = fun.as_list()?;

            if let &Value::Symbol(s) = &funl[0] {
                // If the application is closed
                // there is no need to create a closure and jump to it,
                // just evaluate the arguments in the correct order
                // and continue with the body
                if s == FN {
                    match funl[1].clone() {
                        Value::Symbol(_inner_args) => {
                            unimplemented!();
                        }
                        // If the function has no arguments,
                        // applying it is the same evaluating its body.
                        Value::Nil => {
                            let new_env = AEnv::new(Some(env.clone()));
                            let new_env_ref = Rc::new(new_env);

                            let body = self.preprocess_meaning_sequence(
                                funl[2..].to_vec(),
                                new_env_ref,
                                tail,
                            )?;

                            return Ok(Meaning::ClosedApplicationNil(Box::new(body), tail));
                        }
                        other => {
                            if other.is_true_list() {
                                let inner_args = other.as_list()?;
                                let arity = args.len();
                                if arity != inner_args.len() {
                                    panic!("Invalid arity");
                                }

                                let arg_syms: LispResult<Vec<Symbol>> =
                                    inner_args.into_iter().map(|x| x.as_symbol()).collect();
                                let mut new_env = AEnv::new(Some(env));
                                new_env.extend(arg_syms?);
                                let new_env_ref = Rc::new(new_env);

                                let body = self.preprocess_meaning_sequence(
                                    funl[2..].to_vec(),
                                    new_env_ref,
                                    tail,
                                )?;

                                return Ok(Meaning::ClosedApplication(args, Box::new(body), tail));
                            } else if other.is_pair() {
                                unimplemented!();
                            // Value::DottedList(inner_args, _tail) => {
                            //     if args.len() < inner_args.len() {
                            //         panic!("Invalid arity");
                            //     }
                            // }
                            } else {
                                Err(Error::InvalidFunctionArgument(other))?;
                            }
                        }
                    }
                }
            }
        }

        // Regular application
        let mfun = self.preprocess_meaning(fun, env, false)?;
        Ok(Meaning::RegularApplication(Box::new(mfun), args, tail))
    }
}
