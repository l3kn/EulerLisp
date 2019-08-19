mod constant_folding;
mod error;
mod optimize;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::builtin::BuiltinRegistry;
use crate::env::{AEnv, AEnvRef};
use crate::symbol_table::{self, SymbolTable};
use crate::syntax_rule::SyntaxRule;

use crate::instruction::{Instruction, LabeledInstruction};

use crate::{Arity, LispFnType, LispResult, Symbol, Value};

pub use error::CompilerError;

#[derive(Debug)]
pub enum VariableKind {
    Builtin((LispFnType, u16, Arity)),
    Global(usize),
    Local(usize, usize),
    Constant(usize),
}

pub struct Program {
    pub instructions: Vec<LabeledInstruction>,
    pub constants: Vec<Value>,
    pub num_globals: usize,
}

pub struct Compiler {
    symbol_table: Rc<RefCell<SymbolTable>>,
    syntax_rules: HashMap<Symbol, SyntaxRule>,
    // Mapping from symbols to the constant list for `defconst`
    constant_table: HashMap<String, usize>,
    global_vars: HashMap<String, usize>,
    global_var_index: usize,
    current_uid: usize,
    builtins: BuiltinRegistry,
    constants: Vec<Value>,
}

impl Compiler {
    pub fn new(symbol_table: Rc<RefCell<SymbolTable>>, builtins: BuiltinRegistry) -> Self {
        Compiler {
            symbol_table,
            syntax_rules: HashMap::new(),
            constant_table: HashMap::new(),
            global_vars: HashMap::new(),
            global_var_index: 0,
            current_uid: 0,
            builtins,
            constants: Vec::new(),
        }
    }

    // Used by the repl to introduce a new global variable
    // for the result of the last command
    pub fn bind_global(&mut self, name: &str) {
        self.global_vars
            .insert(name.to_string(), self.global_var_index);
        self.global_var_index += 1;
    }

    pub fn compile(&mut self, mut datums: Vec<Value>, tail: bool) -> Program {
        let global_var_index_before = self.global_var_index;
        let constants_len_before = self.constants.len();

        // NOTE: Chaining the `filter_map` does not work,
        // `defsyntax` should be able to be used before they are defined
        datums = datums
            .into_iter()
            .filter_map(|d| self.extract_macros(d).unwrap())
            .collect();
        datums = datums
            .into_iter()
            .map(|d| self.expand_macros(d).unwrap())
            .collect();
        datums = datums
            .into_iter()
            .filter_map(|d| self.extract_constants(d).unwrap())
            .collect();
        datums = datums
            .into_iter()
            .map(|d| self.convert_outer_defs(d).unwrap())
            .collect();
        datums = datums
            .into_iter()
            .map(|d| self.convert_inner_defs(d).unwrap())
            .collect();
        datums = datums.into_iter().map(constant_folding::fold).collect();

        if datums.is_empty() {
            return Program {
                instructions: vec![],
                constants: self.constants[constants_len_before..].to_vec().clone(),
                num_globals: self.global_var_index - global_var_index_before,
            };
        }

        // FIXME: Handle errors
        let mut instructions = Vec::new();
        let last = datums.len() - 1;
        for (i, d) in datums.into_iter().enumerate() {
            let empty_aenv = AEnv::new(None);
            let aenv_ref = Rc::new(RefCell::new(empty_aenv));
            let labeled_insts = self
                .preprocess_meaning(d, aenv_ref, tail && i == last)
                .unwrap();
            instructions.extend(labeled_insts);
        }

        let optimized = optimize::optimize(instructions);
        Program {
            instructions: optimized,
            constants: self.constants[constants_len_before..].to_vec().clone(),
            num_globals: self.global_var_index - global_var_index_before,
        }
    }

    fn get_uid(&mut self) -> usize {
        self.current_uid += 1;
        self.current_uid
    }

    fn add_constant(&mut self, c: Value) -> usize {
        self.constants
            .iter()
            .position(|x| *x == c)
            .unwrap_or_else(|| {
                let res = self.constants.len();
                self.constants.push(c);
                res
            })
    }

    // Passes:
    // 1: Collect defsyntax expressions
    // 2: Expand macros
    // 3: Collect def expressions
    // TODO: Make distinction between language keywords
    // and builtin functions
    pub fn is_reserved(&mut self, symbol: &str) -> bool {
        match symbol {
            "fn" | "do" | "quote" | "defsyntax" | "def" | "set!" | "if" => true,
            other => self.builtins.contains_key(other),
        }
    }

    pub fn extract_constants(&mut self, datum: Value) -> Result<Option<Value>, CompilerError> {
        if datum.is_true_list() {
            let mut elems = datum.as_pair().unwrap().collect_list().unwrap();
            let name = elems.remove(0);
            if let Value::Symbol(s) = name {
                if s == symbol_table::DEFCONST {
                    let name_sym = elems.remove(0).as_symbol().unwrap();
                    let name = self.symbol_table.borrow().lookup(name_sym);
                    let value = constant_folding::fold(elems.remove(0));

                    if self.is_reserved(&name) {
                        return Err(CompilerError::ReservedName(name));
                    }

                    if !value.is_self_evaluating() {
                        return Err(CompilerError::NonSelfEvaluatingConstant(name));
                    }

                    let idx = self.add_constant(value);
                    self.constant_table.insert(name, idx);
                    return Ok(None);
                }
            }
        }

        Ok(Some(datum))
    }

    pub fn extract_macros(&mut self, datum: Value) -> Result<Option<Value>, CompilerError> {
        if datum.is_true_list() {
            let mut elems = datum.as_pair().unwrap().collect_list().unwrap();
            let name = elems.remove(0);
            if let Value::Symbol(s) = name {
                if s == symbol_table::DEFSYNTAX {
                    let rule_name_sym = elems.remove(0).as_symbol().unwrap();
                    let rule_name = self.symbol_table.borrow().lookup(rule_name_sym);
                    let literals = elems.remove(0).as_list().unwrap();
                    let rules = elems.remove(0).as_list().unwrap();
                    let syntax_rule = SyntaxRule::parse(rule_name_sym, literals, rules);

                    if self.is_reserved(&rule_name) {
                        return Err(CompilerError::ReservedName(rule_name));
                    }
                    self.syntax_rules.insert(rule_name_sym, syntax_rule);
                    return Ok(None);
                }
            }
        }

        Ok(Some(datum))
    }

    pub fn expand_macros(&mut self, datum: Value) -> Result<Value, CompilerError> {
        if datum.is_true_list() {
            let mut elems = datum.as_pair().unwrap().collect_list().unwrap();
            // FIXME: A list should never be empty,
            // how does this happen?
            if elems.is_empty() {
                return Ok(Value::Nil);
            }
            // Wrong order
            let name = elems[0].clone();
            if let Value::Symbol(sym) = name {
                let macro_name = self.symbol_table.borrow().lookup(sym);
                // FIXME: Do this without the clone
                let rules = self.syntax_rules.clone();
                let rule = rules.get(&sym);
                if rule.is_none() {
                    let elems: Result<Vec<Value>, CompilerError> =
                        elems.into_iter().map(|d| self.expand_macros(d)).collect();
                    return Ok(Value::make_list_from_vec(elems?));
                }
                let sr = rule.unwrap().clone();
                elems.remove(0);
                if let Some(ex) = sr.apply(elems.clone()) {
                    self.expand_macros(ex)
                } else {
                    return Err(CompilerError::NoMatchingMacroPattern(
                        Value::make_list_from_vec(elems),
                    ));
                }
            } else {
                let elems: Result<Vec<Value>, CompilerError> =
                    elems.into_iter().map(|d| self.expand_macros(d)).collect();
                return Ok(Value::make_list_from_vec(elems?));
            }
        } else {
            Ok(datum)
        }
    }

    pub fn convert_outer_defs(&mut self, datum: Value) -> Result<Value, CompilerError> {
        if datum.is_true_list() {
            let mut elems = datum.as_pair().unwrap().collect_list().unwrap();
            let name_sym = elems.remove(0);
            if let Value::Symbol(s) = name_sym {
                if s == symbol_table::DEF {
                    let var_name_sym = elems.remove(0).as_symbol().unwrap();
                    let var_name = self.symbol_table.borrow().lookup(var_name_sym);
                    let value = elems.remove(0);

                    if self.is_reserved(&var_name) {
                        return Err(CompilerError::ReservedName(var_name));
                    }

                    if !self.global_vars.contains_key(&var_name) {
                        self.global_vars
                            .insert(var_name.clone(), self.global_var_index);
                        self.global_var_index += 1;
                    }

                    return Ok(Value::make_list_from_vec(vec![
                        Value::Symbol(symbol_table::SET),
                        Value::Symbol(var_name_sym),
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
    pub fn convert_inner_defs(&mut self, datum: Value) -> Result<Value, CompilerError> {
        if datum.is_true_list() {
            let elems = datum.as_pair().unwrap().collect_list().unwrap();
            let res: Result<Vec<Value>, CompilerError> = elems
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
                            let mut b_elems = body.as_pair().unwrap().collect_list().unwrap();
                            let b_name = b_elems.remove(0);
                            if let Value::Symbol(sym) = b_name {
                                if sym == symbol_table::DEF {
                                    if found_non_def {
                                        return Err(CompilerError::InvalidInternalDefinition);
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
                let mut elems = elems_.borrow().collect_list().unwrap();
                let name_sym = elems.remove(0);
                if let Value::Symbol(sym) = name_sym {
                    let name = self.symbol_table.borrow().lookup(sym);
                    match name.as_ref() {
                        // TODO: Check arity
                        "quote" => self.preprocess_meaning_quotation(elems.remove(0), env, tail),
                        "fn" => self.preprocess_meaning_abstraction(elems, env, tail),
                        "if" => self.preprocess_meaning_alternative(elems, env, tail),
                        "do" => self.preprocess_meaning_sequence(elems, env, tail),
                        "set!" => self.preprocess_meaning_assignment(elems, env, tail),
                        _ => {
                            // FIXME: Do this without the clone
                            let rules = self.syntax_rules.clone();
                            let rule = rules.get(&sym);
                            if rule.is_none() {
                                return self
                                    .preprocess_meaning_application(name_sym, elems, env, tail);
                            }
                            let sr = rule.unwrap().clone();
                            match sr.apply(elems.clone()) {
                                Some(ex) => self.preprocess_meaning(ex, env, tail),
                                None => {
                                    return Err(CompilerError::NoMatchingMacroPattern(
                                        Value::make_list_from_vec(elems),
                                    ))?;
                                }
                            }
                        }
                    }
                } else {
                    self.preprocess_meaning_application(name_sym, elems, env, tail)
                }
            }
            Value::Symbol(symbol) => {
                let string = self.symbol_table.borrow().lookup(symbol);
                self.preprocess_meaning_reference(string, env, tail)
            }
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
    fn compute_kind(&self, symbol: String, env: AEnvRef) -> Result<VariableKind, CompilerError> {
        if let Some(binding) = env.borrow().lookup(&symbol) {
            return Ok(VariableKind::Local(binding.0, binding.1));
        }

        if self.global_vars.contains_key(&symbol) {
            return Ok(VariableKind::Global(self.global_vars[&symbol]));
        }

        if self.constant_table.contains_key(&symbol) {
            return Ok(VariableKind::Constant(self.constant_table[&symbol]));
        }

        if let Some(builtin) = self.builtins.get_(&symbol) {
            return Ok(VariableKind::Builtin(builtin.clone()));
        }

        Err(CompilerError::UndefinedVariable(symbol))
    }

    fn preprocess_meaning_reference(
        &mut self,
        symbol: String,
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
            VariableKind::Builtin((typ, index, arity)) => {
                // TODO: In the book builtins are handled in a different way,
                // see page 213
                let c = self.add_constant(Value::Builtin(typ, index, arity));
                Ok(vec![(Instruction::Constant(c as u16), None)])
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
        let string = self.symbol_table.borrow().lookup(symbol);
        let mut res = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;

        match self.compute_kind(string.clone(), env)? {
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
            VariableKind::Builtin(_fun) => {
                // TODO: Only use errors of one kind for all compiler errors?
                Err(CompilerError::ReservedName(string))?
            }
            VariableKind::Constant(_i) => Err(CompilerError::ConstantReassignment(string))?,
        }
    }

    fn preprocess_meaning_quotation(
        &mut self,
        datum: Value,
        _env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        let constant = self.add_constant(datum);
        Ok(vec![(Instruction::Constant(constant as u16), None)])
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
                    let elems = _pair.borrow().collect_list().unwrap();
                    for e in elems {
                        let sym = e.as_symbol().unwrap();
                        let string = self.symbol_table.borrow().lookup(sym).to_string();
                        names.push(string);
                    }
                } else {
                    let elems = _pair.borrow().collect();
                    for e in elems {
                        let sym = e.as_symbol().unwrap();
                        let string = self.symbol_table.borrow().lookup(sym).to_string();
                        names.push(string);
                    }
                    dotted = true
                }
            }
            Value::Symbol(sym) => {
                let string = self.symbol_table.borrow().lookup(sym).to_string();
                names.push(string);
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
        names: Vec<String>,
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

        let env2ref = Rc::new(RefCell::new(env2));
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
        names: Vec<String>,
        body: Vec<Value>,
        env: AEnvRef,
        _tail: bool,
    ) -> LispResult<Vec<LabeledInstruction>> {
        let arity = names.len();
        let mut env2 = AEnv::new(Some(env));
        env2.extend(names);

        let env2ref = Rc::new(RefCell::new(env2));
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
            let c = self.add_constant(Value::Nil);
            vec![(Instruction::Constant(c as u16), None)]
        } else {
            self.preprocess_meaning(datums.remove(0), env, tail)?
        };

        let mut last = alt.pop().unwrap();
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

        if let Value::Symbol(name) = fun {
            let name_str = self.symbol_table.borrow().lookup(name);
            match name_str.as_ref() {
                "inc" | "dec" | "fst" | "rst" | "not" | "is_zero?" | "is_nil?" => {
                    if arity != 1 {
                        return Err(CompilerError::IncorrectPrimitiveArity(
                            name_str.to_string(),
                            1,
                            arity,
                        ))?;
                    }

                    res.extend(args[0].clone());

                    match name_str.as_ref() {
                        "inc" => res.push((Instruction::Inc, None)),
                        "dec" => res.push((Instruction::Dec, None)),
                        "fst" => res.push((Instruction::Fst, None)),
                        "rst" => res.push((Instruction::Rst, None)),
                        "not" => res.push((Instruction::Not, None)),
                        "zero?" => res.push((Instruction::IsZero, None)),
                        "nil?" => res.push((Instruction::IsNil, None)),
                        // TODO: Translate symbol name, throw error
                        other => panic!("Unknown monadic VM primitive {}", other),
                    }
                    return Ok(res);
                }
                "__bin+" | "__bin-" | "__bin*" | "__bin/" | "__bin=" | "__bin<" | "__bin>"
                | "__bin<=" | "__bin>=" | "__binequal?" | "cons" | "!=" | "div" | "%"
                | "vector-ref" => {
                    if arity != 2 {
                        return Err(CompilerError::IncorrectPrimitiveArity(
                            name_str.to_string(),
                            2,
                            arity,
                        ))?;
                    }

                    res.extend(args[1].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[0].clone());
                    res.push((Instruction::PopArg1, None));

                    match name_str.as_ref() {
                        "__bin+" => res.push((Instruction::Add, None)),
                        "__bin-" => res.push((Instruction::Sub, None)),
                        "__bin*" => res.push((Instruction::Mul, None)),
                        "__bin/" => res.push((Instruction::Div, None)),
                        "div" => res.push((Instruction::IntDiv, None)),
                        "%" => res.push((Instruction::Mod, None)),
                        "__bin=" => res.push((Instruction::Eq, None)),
                        "!=" => res.push((Instruction::Neq, None)),
                        "__bin<" => res.push((Instruction::Lt, None)),
                        "__bin>" => res.push((Instruction::Gt, None)),
                        "__bin<=" => res.push((Instruction::Lte, None)),
                        "__bin>=" => res.push((Instruction::Gte, None)),
                        "__binequal?" => res.push((Instruction::Equal, None)),
                        "cons" => res.push((Instruction::Cons, None)),
                        "vector-ref" => res.push((Instruction::VectorRef, None)),
                        other => panic!("Unknown binary VM primitive {}", other),
                    }
                    return Ok(res);
                }
                "vector-set!" => {
                    if arity != 3 {
                        return Err(CompilerError::IncorrectPrimitiveArity(
                            name_str.to_string(),
                            3,
                            arity,
                        ))?;
                    }

                    res.extend(args[2].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[1].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[0].clone());
                    res.push((Instruction::PopArg1, None));
                    res.push((Instruction::PopArg2, None));

                    match name_str.as_ref() {
                        "vector-set!" => res.push((Instruction::VectorSet, None)),
                        other => panic!("Unknown ternary VM primitive {}", other),
                    }
                    return Ok(res);
                }
                _ => {}
            }
            if let Some(&(ref t, i, ref ar)) =
                self.builtins.get_(&self.symbol_table.borrow().lookup(name))
            {
                ar.check(arity);
                match t {
                    LispFnType::Variadic => {
                        for e in args {
                            res.extend(e);
                            res.push((Instruction::PushValue, None));
                        }
                        res.push((Instruction::CallN(i as u16, arity as u8), None));
                    }
                    LispFnType::Fixed1 => {
                        res.extend(args[0].clone());
                        res.push((Instruction::Call1(i as u16), None));
                    }
                    LispFnType::Fixed2 => {
                        res.extend(args[1].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[0].clone());
                        res.push((Instruction::PopArg1, None));
                        res.push((Instruction::Call2(i as u16), None));
                    }
                    LispFnType::Fixed3 => {
                        res.extend(args[2].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[1].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[0].clone());
                        res.push((Instruction::PopArg1, None));
                        res.push((Instruction::PopArg2, None));
                        res.push((Instruction::Call3(i as u16), None));
                    }
                }

                return Ok(res);
            }
        }

        if fun.is_true_list() {
            let funl = fun.as_pair().unwrap().collect_list().unwrap();

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
                            let new_env_ref = Rc::new(RefCell::new(new_env));

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
                                let inner_args = other.as_pair().unwrap().collect_list().unwrap();
                                let arity = args.len();
                                if arity != inner_args.len() {
                                    panic!("Invalid arity");
                                }

                                for e in args.into_iter().rev() {
                                    res.extend(e);
                                    res.push((Instruction::PushValue, None))
                                }
                                res.push((Instruction::AllocateFillFrame(arity as u8), None));

                                let arg_strs: Vec<String> = inner_args
                                    .into_iter()
                                    .map(|x| x.as_symbol().unwrap())
                                    .map(|x| self.symbol_table.borrow().lookup(x))
                                    .collect();
                                let mut new_env = AEnv::new(Some(env));
                                new_env.extend(arg_strs);
                                let new_env_ref = Rc::new(RefCell::new(new_env));

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
            // because this would restore it __before__ the clojure,
            // called by setting `vm.pc`, is executed.
            res.push((Instruction::RestoreEnv, None));
        }

        Ok(res)
    }
}
