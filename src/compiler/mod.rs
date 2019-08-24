mod constant_folding;
mod error;
mod optimize;

use std::collections::HashMap;
use std::rc::Rc;

use crate::env::{AEnv, AEnvRef};
use crate::instruction::{Instruction, LabeledInstruction, INTEGER_INST_MAX};
use crate::symbol_table::{self, Symbol};
use crate::syntax_rule::SyntaxRule;
use crate::vm::Context;
use crate::{LispResult, Value};

pub use error::CompilerError;

#[derive(Debug)]
pub enum VariableKind {
    Global(usize),
    Local(usize, usize),
    Constant(usize),
}

#[derive(Debug)]
pub struct Program {
    pub instructions: Vec<LabeledInstruction>,
}

pub struct Compiler {
    syntax_rules: HashMap<Symbol, SyntaxRule>,
    // Mapping from symbols to the constant list for `defconst`
    current_uid: usize,
    context: Rc<Context>,
}

impl Compiler {
    pub fn new(context: Rc<Context>) -> Self {
        Compiler {
            syntax_rules: HashMap::new(),
            current_uid: 0,
            context,
        }
    }

    pub fn compile(&mut self, mut datums: Vec<Value>, tail: bool) -> LispResult<Program> {
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

        if datums.is_empty() {
            return Ok(Program {
                instructions: vec![],
            });
        }

        // FIXME: Handle errors
        let mut instructions = Vec::new();
        let last = datums.len() - 1;
        for (i, d) in datums.into_iter().enumerate() {
            let empty_aenv = AEnv::new(None);
            let aenv_ref = Rc::new(empty_aenv);
            let labeled_insts = self.preprocess_meaning(d, aenv_ref, tail && i == last)?;
            instructions.extend(labeled_insts);
        }

        let optimized = optimize::optimize(instructions);

        Ok(Program {
            instructions: optimized,
        })
    }

    fn get_uid(&mut self) -> usize {
        self.current_uid += 1;
        self.current_uid
    }

    // Passes:
    // 1: Collect defsyntax expressions
    // 2: Expand macros
    // 3: Collect def expressions
    // TODO: Make distinction between language keywords
    // and builtin functions
    pub fn is_reserved(&mut self, symbol: Symbol) -> bool {
        // NOTE: This assumes a fixed ordering of reserved symbols
        symbol < symbol_table::LET
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
                        Err(CompilerError::ReservedName(name_sym))?;
                    }

                    if !value.is_self_evaluating() {
                        Err(CompilerError::NonSelfEvaluatingConstant(name_sym))?;
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
                        Err(CompilerError::ReservedName(rule_sym))?;
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
                    Err(CompilerError::NoMatchingMacroPattern(
                        Value::make_list_from_vec(elems),
                    ))?
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
                        Err(CompilerError::ReservedName(var_sym))?;
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
                                        Err(CompilerError::InvalidInternalDefinition)?;
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

    pub fn preprocess_meaning(
        &mut self,
        datum: Value,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        match datum {
            Value::Pair(ref elems_) if datum.is_true_list() => {
                let mut elems = elems_.borrow().collect_list()?;
                let name_sym = elems.remove(0);

                if let Value::Symbol(sym) = name_sym {
                    if sym == symbol_table::QUOTE {
                        self.preprocess_meaning_quotation(elems.remove(0), env, tail)
                    } else if sym == symbol_table::FN {
                        self.preprocess_meaning_abstraction(elems, env, tail)
                    } else if sym == symbol_table::IF {
                        self.preprocess_meaning_alternative(elems, env, tail)
                    } else if sym == symbol_table::DO {
                        self.preprocess_meaning_sequence(elems, env, tail)
                    } else if sym == symbol_table::SET {
                        self.preprocess_meaning_assignment(elems, env, tail)
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
                                return Err(CompilerError::NoMatchingMacroPattern(
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

    // A variable can have three types:
    // - builtin, non overwritable
    // - defined in the global env
    // - defined in some local env
    //
    // Local variables can shadow global & builtin variables,
    // Global variables can shadow builtin variables.
    fn compute_kind(&self, symbol: Symbol, env: AEnvRef) -> LispResult<VariableKind> {
        if let Some(binding) = env.lookup(symbol) {
            return Ok(VariableKind::Local(binding.0, binding.1));
        }

        if let Some(index) = self.context.lookup_global_index(symbol) {
            return Ok(VariableKind::Global(index));
        }

        if let Some(index) = self.context.lookup_constant_index(symbol) {
            return Ok(VariableKind::Constant(index));
        }

        Err(CompilerError::UndefinedVariable(symbol))?
    }

    fn preprocess_meaning_reference(
        &mut self,
        symbol: Symbol,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        match self.compute_kind(symbol, env)? {
            VariableKind::Local(i, j) => {
                if i == 0 {
                    Ok(vec![(Instruction::ShallowArgumentRef(j as u16), None)])
                } else {
                    Ok(vec![(
                        Instruction::DeepArgumentRef(i as u16, j as u16),
                        None,
                    )])
                }
            }
            VariableKind::Global(j) => {
                // Checked because the variable might be used before it has been defined
                Ok(vec![(Instruction::CheckedGlobalRef(j as u16), None)])
            }
            VariableKind::Constant(i) => Ok(vec![(Instruction::Constant(i as u16), None)]),
        }
    }

    fn preprocess_meaning_assignment(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        // TODO: Check arity
        let symbol = datums.remove(0).as_symbol()?;
        let mut res = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;

        match self.compute_kind(symbol, env)? {
            VariableKind::Local(i, j) => {
                if i == 0 {
                    res.push((Instruction::ShallowArgumentSet(j as u16), None));
                } else {
                    res.push((Instruction::DeepArgumentSet(i as u16, j as u16), None));
                }
                Ok(res)
            }
            VariableKind::Global(j) => {
                // Checked because the variable might be used before it has been defined
                res.push((Instruction::GlobalSet(j as u16), None));
                Ok(res)
            }
            VariableKind::Constant(_i) => Err(CompilerError::ConstantReassignment(symbol))?,
        }
    }

    fn preprocess_meaning_quotation(
        &mut self,
        datum: Value,
        _env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        match datum {
            Value::Integer(i) if i >= 0 && i <= INTEGER_INST_MAX => {
                Ok(vec![(Instruction::Integer(i as u16), None)])
            }
            other => {
                let constant = self.context.add_anonymous_constant(other);
                Ok(vec![(Instruction::Constant(constant as u16), None)])
            }
        }
    }

    fn preprocess_meaning_abstraction(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        let names_ = datums.remove(0);
        let mut dotted = false;
        let mut names = Vec::new();

        let true_list = names_.is_true_list();
        match names_ {
            Value::Pair(_pair) => {
                if true_list {
                    let elems = _pair.borrow().collect_list()?;
                    for e in elems {
                        let sym = e.as_symbol()?;
                        names.push(sym);
                    }
                } else {
                    let elems = _pair.borrow().collect();
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
            _ => panic!("First argument to fn must be a list or a symbol"),
        }

        // TODO: Is there even a difference?
        if dotted {
            self.preprocess_meaning_dotted_abstraction(names, datums, env, tail)
        } else {
            self.preprocess_meaning_fix_abstraction(names, datums, env, tail)
        }
    }

    // TODO: Combine these two into one method
    fn preprocess_meaning_fix_abstraction(
        &mut self,
        names: Vec<Symbol>,
        body: Vec<Value>,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        /*
         * CREATE-CLOSURE -\
         * GOTO            | -\
         * body...        <-  |
         *                  <-/
         */

        let arity = names.len();
        let mut env2 = AEnv::new(Some(env));
        env2.extend(names);

        let env2ref = Rc::new(env2);
        let body = self.preprocess_meaning_sequence(body, env2ref, true)?;

        let label = self.get_uid();

        let mut res = vec![
            (Instruction::FixClosure(arity as u16), None),
            (Instruction::Jump(label as u32), None),
        ];
        res.extend(body);
        res.push((Instruction::Return, Some(label)));

        Ok(res)
    }
    fn preprocess_meaning_dotted_abstraction(
        &mut self,
        names: Vec<Symbol>,
        body: Vec<Value>,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        let arity = names.len();
        let mut env2 = AEnv::new(Some(env));
        env2.extend(names);

        let env2ref = Rc::new(env2);
        let body = self.preprocess_meaning_sequence(body, env2ref, true)?;
        let label = self.get_uid();

        let mut res = vec![
            (Instruction::DottedClosure(arity as u16), None),
            (Instruction::Jump(label as u32), None),
        ];
        res.extend(body);
        res.push((Instruction::Return, Some(label)));

        Ok(res)
    }

    /// test
    /// JUMP_FALSE -\
    /// cons        |
    /// JUMP        | -\
    /// alt       <-/  |
    /// ...          <-/
    fn preprocess_meaning_alternative(
        &mut self,
        mut datums: Vec<Value>,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        let mut test = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;
        let mut cons = self.preprocess_meaning(datums.remove(0), env.clone(), tail)?;

        let mut alt = if datums.is_empty() {
            let c = self.context.add_anonymous_constant(Value::Nil);
            vec![(Instruction::Constant(c as u16), None)]
        } else {
            self.preprocess_meaning(datums.remove(0), env, tail)?
        };

        let mut last = alt.pop()?;
        let alt_label = if let Some(l) = last.1 {
            l
        } else {
            let l = self.get_uid();
            last.1 = Some(l);
            l
        };
        alt.push(last);

        let cons_label = self.get_uid();
        cons.push((Instruction::Jump(alt_label as u32), Some(cons_label)));

        test.push((Instruction::JumpFalse(cons_label as u32), None));

        test.extend(cons);
        test.extend(alt);

        Ok(test)
    }

    fn preprocess_meaning_sequence(
        &mut self,
        datums: Vec<Value>,
        env: AEnvRef,
        tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        let mut res = Vec::new();

        if !datums.is_empty() {
            let last = datums.len() - 1;
            for (i, d) in datums.into_iter().enumerate() {
                let m = self.preprocess_meaning(d, env.clone(), (i == last) && tail)?;
                res.extend(m);
            }
        }

        Ok(res)
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
    ) -> LispResult<Vec<LabeledInstruction>> {
        let mut args: Vec<Vec<LabeledInstruction>> = Vec::new();

        for d in datums {
            let res = self.preprocess_meaning(d, env.clone(), false)?;
            args.push(res);
        }

        // TODO: At some point it would be nice to allow predifined variables, too.
        // Once this is implemented, there needs to be a check here.
        let mut res = Vec::new();
        let arity = args.len();

        use symbol_table::*;

        if let Value::Symbol(name) = fun {
            match name {
                INC | DEC | FST | RST | NOT | IS_ZERO | IS_NIL | CALL_CC | EVAL => {
                    if arity != 1 {
                        return Err(CompilerError::IncorrectPrimitiveArity(name, 1, arity))?;
                    }

                    res.extend(args[0].clone());

                    match name {
                        INC => res.push((Instruction::Inc, None)),
                        DEC => res.push((Instruction::Dec, None)),
                        FST => res.push((Instruction::Fst, None)),
                        RST => res.push((Instruction::Rst, None)),
                        NOT => res.push((Instruction::Not, None)),
                        IS_ZERO => res.push((Instruction::IsZero, None)),
                        IS_NIL => res.push((Instruction::IsNil, None)),
                        CALL_CC => res.push((Instruction::CallCC, None)),
                        EVAL => res.push((Instruction::Eval, None)),
                        // TODO: Translate symbol name, throw error
                        other => panic!("Unknown monadic VM primitive {}", other),
                    }
                    return Ok(res);
                }
                BIN_EQ | BIN_EQUAL | BIN_LT | BIN_LTE | BIN_GT | BIN_GTE | NE | CONS | MOD
                | VECTOR_REF | APPLY => {
                    if arity != 2 {
                        return Err(CompilerError::IncorrectPrimitiveArity(name, 2, arity))?;
                    }

                    res.extend(args[1].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[0].clone());
                    res.push((Instruction::PopArg1, None));

                    match name {
                        BIN_EQ => res.push((Instruction::Eq, None)),
                        BIN_EQUAL => res.push((Instruction::Equal, None)),
                        BIN_LT => res.push((Instruction::Lt, None)),
                        BIN_LTE => res.push((Instruction::Lte, None)),
                        BIN_GT => res.push((Instruction::Gt, None)),
                        BIN_GTE => res.push((Instruction::Gte, None)),
                        MOD => res.push((Instruction::Mod, None)),
                        NE => res.push((Instruction::Neq, None)),
                        CONS => res.push((Instruction::Cons, None)),
                        VECTOR_REF => res.push((Instruction::VectorRef, None)),
                        APPLY => res.push((Instruction::Apply, None)),
                        other => panic!("Unknown binary VM primitive {}", other),
                    }
                    return Ok(res);
                }
                ADD | SUB | MUL | DIV | IDIV => {
                    if arity < 2 {
                        // TODO: Allow Min(2) as arity in the error
                        return Err(CompilerError::IncorrectPrimitiveArity(name, 2, arity))?;
                    }

                    let instruction = match name {
                        ADD => Instruction::Add,
                        SUB => Instruction::Sub,
                        MUL => Instruction::Mul,
                        DIV => Instruction::Div,
                        IDIV => Instruction::IntDiv,
                        other => panic!("Unknown binary VM primitive {}", other),
                    };

                    for i in (1..args.len()).rev() {
                        res.extend(args[i].clone());
                        res.push((Instruction::PushValue, None));
                    }
                    res.extend(args[0].clone());
                    for _i in 0..(args.len() - 1) {
                        res.push((Instruction::PopArg1, None));
                        res.push((instruction, None));
                    }

                    return Ok(res);
                }
                VECTOR_SET => {
                    if arity != 3 {
                        return Err(CompilerError::IncorrectPrimitiveArity(name, 3, arity))?;
                    }

                    res.extend(args[2].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[1].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[0].clone());
                    res.push((Instruction::PopArg1, None));
                    res.push((Instruction::PopArg2, None));

                    match name {
                        VECTOR_SET => res.push((Instruction::VectorSet, None)),
                        other => panic!("Unknown ternary VM primitive {}", other),
                    }
                    return Ok(res);
                }
                VAR_ADD => {}
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
                if s == symbol_table::FN {
                    match funl[1].clone() {
                        Value::Symbol(_inner_args) => {
                            unimplemented!();
                        }
                        Value::Nil => {
                            let new_env = AEnv::new(Some(env.clone()));
                            let new_env_ref = Rc::new(new_env);

                            let body = self.preprocess_meaning_sequence(
                                funl[2..].to_vec(),
                                new_env_ref,
                                tail,
                            )?;
                            res.push((Instruction::ExtendEnv, None));
                            res.extend(body);
                            if !tail {
                                res.push((Instruction::UnlinkEnv, None));
                            }
                            return Ok(res);
                        }
                        other => {
                            if other.is_true_list() {
                                let inner_args = other.as_list()?;
                                let arity = args.len();
                                if arity != inner_args.len() {
                                    panic!("Invalid arity");
                                }

                                for e in args.into_iter().rev() {
                                    res.extend(e);
                                    res.push((Instruction::PushValue, None))
                                }
                                res.push((Instruction::AllocateFillFrame(arity as u8), None));

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

                                res.push((Instruction::ExtendEnv, None));
                                res.extend(body);
                                if !tail {
                                    res.push((Instruction::UnlinkEnv, None));
                                }

                                return Ok(res);
                            } else if other.is_pair() {
                                unimplemented!();
                            // Value::DottedList(inner_args, _tail) => {
                            //     if args.len() < inner_args.len() {
                            //         panic!("Invalid arity");
                            //     }
                            // }
                            } else {
                                Err(CompilerError::InvalidFunctionArgument(other))?;
                            }
                        }
                    }
                }
            }
        }

        // Regular application
        let mfun = self.preprocess_meaning(fun, env, false)?;
        res.extend(mfun);
        res.push((Instruction::PushValue, None));

        // Arguments must be stored on the stack first,
        // otherwise they would be overwritten during nested applications
        let arity = args.len();
        for e in args.into_iter().rev() {
            res.extend(e);
            res.push((Instruction::PushValue, None))
        }

        // Since this is the only place FunctionInvoke is used,
        // for performance reasons AllocateFillFrame and PopFunction
        // are already included
        //
        // res.push((Instruction::AllocateFillFrame(arity as u8), None));
        // res.push((Instruction::PopFunction, None));

        if tail {
            res.push((Instruction::FunctionInvoke(true, arity as u8), None));
        } else {
            // For tail FunctionInvoke, the environment is preserved automatically
            res.push((Instruction::FunctionInvoke(false, arity as u8), None));
            // Can't restore the env in the instruction,
            // because this would restore it __before__ the closure,
            // called by setting `vm.pc`, is executed.
            res.push((Instruction::RestoreEnv, None));
        }

        Ok(res)
    }
}
