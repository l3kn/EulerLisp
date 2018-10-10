mod instruction;
pub mod vm;
mod optimize;
mod constant_folding;

use std::fs;
use std::fs::File;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::io::{Read, Write};

use env::{AEnv, AEnvRef};
use parser::Parser;
use symbol_table::SymbolTable;
use syntax_rule::SyntaxRule;
use builtin;
use builtin::BuiltinRegistry;

use ::Datum;
use ::Expression;
use ::LispErr;
use ::LispFnType;
use ::Arity;

use self::instruction::Instruction;
use self::vm::{VM};

pub struct Debugger {
    compiler: Compiler,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Datum>
}

type LabeledInstruction = (Instruction, Option<usize>);

impl Debugger {
    pub fn new(stdlib: bool) -> Self {
        let symbol_table = SymbolTable::new();
        let st_ref = Rc::new(RefCell::new(symbol_table));

        let mut registry = BuiltinRegistry::new();
        builtin::load(&mut registry);

        let mut eval = Debugger {
            compiler: Compiler::new(st_ref.clone(), registry),
            symbol_table: st_ref,
            constants: Vec::new(),
        };

        if stdlib { eval.load_stdlib(); }

        eval
    }

    fn load_stdlib(&mut self) {
        let mut full = String::new();

        // TODO: Is there a more elegant way to do this?
        let paths = fs::read_dir("./stdlib").unwrap();
        let mut string_paths: Vec<String> = paths.map(
            |p| p.unwrap().path().display().to_string()
        ).collect();
        string_paths.sort();
        for path in string_paths {
            let mut f = File::open(path).expect("Could not open file");
            let mut input = String::new();
            f.read_to_string(&mut input).expect("Could not read file");
            full += &input;
        }

        let mut parser = Parser::from_string(&full);

        // TODO: convert parser errors to lisp errors
        let mut datums : Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        // Ingnore the results,
        // just make sure the compiler contains all the macros & globals
        // from the stdlib
        let Program {
            instructions,
            constants,
            num_globals
        } = self.compiler.compile(datums, true);

        self.constants.extend(constants);
    }

    pub fn debug_file(&mut self, path: &str) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input).expect("Could not read file");

        let mut parser = Parser::from_string(&input);

        // TODO: convert parser errors to lisp errors
        let mut datums : Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        let Program {
            instructions,
            constants,
            num_globals
        } = self.compiler.compile(datums, true);

        self.constants.extend(constants);

        println!("New Globals: {}", num_globals);
        println!("Instructions:");

        // TODO: Add some other way to debug files 
        // as human readable instruction sequences
        // for (i, e) in instructions.iter().enumerate() {
        //     println!("  {:4}: {}", i, self.prettyprint(e));
        // }
    }

    pub fn prettyprint(&self, inst: &Instruction) -> String {
        let st = self.symbol_table.borrow();
        match *inst {
            Instruction::Constant(i) => {
              format!("CONSTANT {}",
                      self.constants[i as usize].to_string(&st))
            },
            Instruction::PushConstant(i) => {
              format!("PUSH-CONSTANT {}",
                      self.constants[i as usize].to_string(&st))
            },
            _ => format!("{}", inst)
        }
    }
}

pub struct Evaluator {
    compiler: Compiler,
    vm: VM,
    pub symbol_table: Rc<RefCell<SymbolTable>>
}

impl Evaluator {
    pub fn new(output: Rc<RefCell<Write>>, stdlib: bool) -> Self {
        let symbol_table = SymbolTable::new();
        let st_ref = Rc::new(RefCell::new(symbol_table));

        let mut registry = BuiltinRegistry::new();
        builtin::load(&mut registry);

        let vm = VM::new(
            output,
            st_ref.clone(),
            registry.clone()
        );

        let mut eval = Evaluator {
            compiler: Compiler::new(st_ref.clone(), registry),
            vm,
            symbol_table: st_ref
        };

        if stdlib { eval.load_stdlib(); }

        eval
    }

    fn load_stdlib(&mut self) {
        let mut full = String::new();

        // TODO: Is there a more elegant way to do this?
        let paths = fs::read_dir("./stdlib").unwrap();
        let mut string_paths: Vec<String> = paths.map(
            |p| p.unwrap().path().display().to_string()
        ).collect();
        string_paths.sort();
        for path in string_paths {
            let mut f = File::open(path).expect("Could not open file");
            let mut input = String::new();
            f.read_to_string(&mut input).expect("Could not read file");
            full += &input;
        }

        self.load_str(&full[..], false);
    }

    pub fn load_file(&mut self, path: &str) {
        // TODO: Add IOError type
        let mut file = File::open(path).expect("Could not open file");
        let mut input = String::new();
        file.read_to_string(&mut input).expect("Could not read file");

        self.load_str(&input[..], true);
    }

    fn load_str(&mut self, input: &str, tail: bool) {
        let string = String::from(input);
        let mut parser = Parser::from_string(&string);

        // TODO: convert parser errors to lisp errors
        let mut datums : Vec<Expression> = Vec::new();
        while let Some(next) = parser.next_expression().expect("Failed to parse") {
            datums.push(next)
        }

        let Program {
            instructions,
            constants,
            num_globals
        } = self.compiler.compile(datums, tail);

        self.vm.append_instructions(instructions);
        self.vm.append_constants(constants);
        self.vm.reserve_global_vars(num_globals);
    }

    pub fn eval_str(&mut self, input: &str) -> Result<Datum, LispErr> {
        // To make the REPL work, jump to the end of the old program
        // every time new code is evaluated
        let start = self.vm.bytecode.len();
        self.load_str(input, false);
        self.vm.set_pc(start as usize);
        self.run();
        return Ok(self.vm.val.take());
    }

    pub fn bind_global(&mut self, name: String, val: Datum) {
        self.compiler.bind_global(name);
        self.vm.add_global(val);
    }

    pub fn run(&mut self) {
        if let Err(err) = self.vm.run() {
            println!("Err: {}", err);
        }
    }

}

#[derive(Debug)]
pub enum VariableKind {
    Builtin((LispFnType, u32, Arity)),
    Global(usize),
    Local(usize, usize),
    Constant(usize),
}

pub struct Program {
    instructions: Vec<u8>,
    constants: Vec<Datum>,
    num_globals: usize,
}

/// Rewrite labeled jumps to relative jumps
///
/// Because the optimization pass can remove instructions
/// all the jumps need to be updated.
/// To do so, all the compiler functions produce `LabeledInstruction`s
/// pairs of an `Instruction` and an `Option<usize>`.
/// The second element is a unique jump label.
/// Jumps point to these labels instead of the final offset.
/// 
/// If a instruction is labeled with `i`
/// `Jump(i)` should jump __behind__ it.
/// 
/// Optimizations run on a vector of `LabeledInstruction`s,
/// then all jumps are rewritten to use relative offsets
/// and the `LabeledInstruction` are converted to normal `Instruction`s.
fn rewrite_jumps(linsts: Vec<LabeledInstruction>) -> Vec<u8> {
    let mut res = vec![];

    // Mapping from label id to instruction index
    let mut labels : HashMap<u32, u32> = HashMap::new();
    let mut pos = 0;

    for &(ref inst, ref label) in linsts.iter() {
        pos += inst.size();
        if let &Some(idx) = label {
            labels.insert(idx as u32, pos as u32);
        }
    }

    // For each jump, look up the index of its label
    // and rewrite it as a relative jump.
    pos = 0;
    for (inst, _label) in linsts.into_iter() {
        pos += inst.size();
        let i = pos as u32;
        match inst {
            Instruction::Jump(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::Jump(to - i).encode());
            },
            Instruction::JumpFalse(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpFalse(to - i).encode());
            },
            Instruction::JumpTrue(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpTrue(to - i).encode());
            },
            Instruction::JumpNil(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpNil(to - i).encode());
            },
            Instruction::JumpNotNil(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpNotNil(to - i).encode());
            },
            Instruction::JumpZero(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpZero(to - i).encode());
            },
            Instruction::JumpNotZero(to_label) => {
                let to = labels.get(&to_label).unwrap();
                res.extend(Instruction::JumpNotZero(to - i).encode());
            },
            other => res.extend(other.encode()),
        }

    }

    return res;
}

pub struct Compiler {
    symbol_table: Rc<RefCell<SymbolTable>>,
    syntax_rules: HashMap<String, SyntaxRule>,
    // Mapping from symbols to the constant list for `defconst`
    constant_table: HashMap<String, usize>,
    // builtins: HashMap<String, LispFn>,
    global_vars: HashMap<String, usize>,
    global_var_index: usize,
    current_uid: usize,
    builtins: BuiltinRegistry,
    constants: Vec<Datum>,
}

impl Compiler {
    pub fn new(symbol_table: Rc<RefCell<SymbolTable>>,
               builtins: BuiltinRegistry) -> Self {

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
    pub fn bind_global(&mut self, name: String) {
        self.global_vars.insert(name.clone(), self.global_var_index);
        self.global_var_index += 1;
    }

    pub fn compile(&mut self, mut datums: Vec<Expression>, tail: bool) -> Program {
        let global_var_index_before = self.global_var_index;
        let constants_len_before = self.constants.len();

        // NOTE: Chaining the `filter_map` does not work,
        // `defsyntax` should be able to be used before they are defined
        datums = datums.into_iter().filter_map(|d| self.extract_macros(d) ).collect();
        datums = datums.into_iter().map(|d| self.expand_macros(d) ).collect();
        datums = datums.into_iter().filter_map(|d| self.extract_constants(d) ).collect();
        datums = datums.into_iter().map(|d| self.convert_outer_defs(d) ).collect();
        datums = datums.into_iter().map(|d| self.convert_inner_defs(d) ).collect();
        datums = datums.into_iter().map(|d| constant_folding::fold(d) ).collect();

        if datums.len() == 0 {
            return Program {
                instructions: vec![],
                constants: self.constants[constants_len_before..].to_vec().clone(),
                num_globals: self.global_var_index - global_var_index_before
            }
        }

        // FIXME: Handle errors
        let mut instructions = Vec::new();
        let last = datums.len() - 1;
        for (i, d) in datums.into_iter().enumerate() {
            let empty_aenv = AEnv::new(None);
            let aenv_ref = Rc::new(RefCell::new(empty_aenv));
            let labeled_insts = self.preprocess_meaning(d, aenv_ref, tail && i == last).unwrap();
            instructions.extend(labeled_insts);
        }

        let optimized = optimize::optimize(instructions);
        Program {
            instructions: rewrite_jumps(optimized),
            constants: self.constants[constants_len_before..].to_vec().clone(),
            num_globals: self.global_var_index - global_var_index_before
        }
    }

    fn get_uid(&mut self) -> usize {
        self.current_uid += 1;
        self.current_uid
    }

    fn add_constant(&mut self, c: Datum) -> usize {
        if let Some(i) = self.constants.iter().position(|x| *x == c) {
            i
        } else {
            let res = self.constants.len();
            self.constants.push(c);
            res
        }
    }

    /**
     * Passes
     * 1: Collect defsyntax expressions
     * 2: Expand macros
     * 3: Collect def expressions
     */

    // TODO: Make distinction between language keywords
    // and builtin functions
    pub fn is_reserved(&mut self, symbol : &str) -> bool {
        match symbol {
            "fn" | "do" | "quote" | "defsyntax" |
            "def" | "set!" | "if" => true,
            other => self.builtins.contains_key(other)
        }
    }

    pub fn extract_constants(&mut self, datum: Expression) -> Option<Expression> {
        match datum.clone() {
            Expression::List(mut elems) => {
                let name = elems.remove(0);
                match name {
                    Expression::Symbol(s) => {
                        match s.as_ref() {
                            "defconst" => {
                                let name = elems.remove(0).as_symbol().unwrap();

                                let value;
                                {
                                    let mut st = self.symbol_table.borrow_mut();

                                    // Allow `defconstants` with expressions that can be folded
                                    let folded = constant_folding::fold(elems.remove(0));
                                    value = folded.to_datum(&mut st);
                                }

                                // TODO: Use `Result` return type
                                if self.is_reserved(&name) {
                                    panic!("{} is a reserved name", name);
                                }

                                if !value.is_self_evaluating() {
                                    panic!("constant {} must be self-evaluating", name);
                                }

                                let idx = self.add_constant(value);
                                self.constant_table.insert(name, idx);
                                None
                            },
                            _ => Some(datum)
                        }
                    },
                    _ => Some(datum)
                }
            },
            other => Some(other)
        }
    }

    pub fn extract_macros(&mut self, datum: Expression) -> Option<Expression> {
        match datum.clone() {
            Expression::List(mut elems) => {
                let name = elems.remove(0);
                match name {
                    Expression::Symbol(s) => {
                        match s.as_ref() {
                            "defsyntax" => {
                                let name = elems.remove(0).as_symbol().unwrap();
                                let literals = elems.remove(0).as_list().unwrap();
                                let rules = elems.remove(0).as_list().unwrap();
                                let syntax_rule = SyntaxRule::parse(name.clone(), literals, rules);

                                // TODO: Use `Result` return type
                                if self.is_reserved(&name) {
                                    panic!("{} is a reserved name", name);
                                }

                                self.syntax_rules.insert(name, syntax_rule);
                                None
                            },
                            _ => Some(datum)
                        }
                    },
                    _ => Some(datum)
                }
            },
            other => Some(other)
        }
    }

    pub fn expand_macros(&mut self, datum: Expression) -> Expression {
        match datum {
            // TODO: Implement macro expansion for dotted lists
            Expression::List(mut elems) => {
                // FIXME: A list should never be empty,
                // how does this happen?
                if elems.len() == 0 {
                    return Expression::Nil;
                }
                // Wrong order
                let name = elems[0].clone();
                if let Expression::Symbol(ref s) = name {
                    // FIXME: Do this without the clone
                    let rules = self.syntax_rules.clone();
                    let rule = rules.get(s).clone();
                    if rule.is_none() {
                        let elems: Vec<Expression> = elems.into_iter().map( |d|
                            self.expand_macros(d)
                        ).collect();
                        return Expression::List(elems);
                    }
                    let sr = rule.unwrap().clone();
                    elems.remove(0);
                    match sr.apply(elems.clone()) {
                        Some(ex) => self.expand_macros(ex),
                        None => {
                            panic!("No matching macro pattern for {}", Expression::List(elems));
                        }
                    }
                } else {
                    let elems: Vec<Expression> = elems.into_iter().map( |d|
                        self.expand_macros(d)
                    ).collect();
                    return Expression::List(elems);
                }
            },
            other => other
        }
    }

    pub fn convert_outer_defs(&mut self, datum : Expression) -> Expression {
        match datum.clone() {
            Expression::List(mut elems) => {
                let name = elems.remove(0);
                match name {
                    Expression::Symbol(ref s) => {
                        match s.as_ref() {
                            "def" => {
                                let name : String = elems.remove(0).as_symbol().unwrap();
                                let value = elems.remove(0);

                                // TODO: Use `Result` return type
                                if self.is_reserved(&name) {
                                    panic!("{} is a reserved name", name);
                                }

                                if !self.global_vars.contains_key(&name) {
                                    self.global_vars.insert(name.clone(), self.global_var_index);
                                    self.global_var_index += 1;
                                }

                                Expression::List(vec![
                                    Expression::Symbol(String::from("set!")),
                                    Expression::Symbol(name.to_string()),
                                    value
                                ])
                            },
                            _ => datum,
                        }
                    },
                    _ => datum,
                }
            },
            other => other
        }
    }

    /**
     * Convert function bodies with internal definitions
     * (only allowed at the top)
     * to a equivalent let(rec) expression
     *
     * ``` scheme
     * (def foo (fn (a)
     *   (def n 100)
     *   (def bar (fn (b) (+ b n)))
     *   (bar a)))
     * ```
     *
     * is converted to
     *
     * ``` scheme
     * (def foo (fn (a)
     *   (let ((n #undefined)
     *         (bar #undefined))
     *      (set! n 100)
     *      (set! bar (fn (b) (+ b n)))
     *      (bar a))))
     * ```
     *
     */
    pub fn convert_inner_defs(&mut self, datum : Expression) -> Expression {
        if let Expression::List(mut elems) = datum {
            elems = elems.into_iter().map(|d| self.convert_inner_defs(d)).collect();
            if let Expression::Symbol(s) = elems[0].clone() {
                if s == "fn" {
                    elems.remove(0);
                    let args = elems.remove(0);

                    let mut defs = Vec::new();
                    let mut bodies = Vec::new();
                    let mut found_non_def = false;

                    for body in elems.iter() {
                        if let &Expression::List(ref b_elems) = body {
                            let mut b_elems = b_elems.clone();
                            let b_name = b_elems.remove(0);
                            if let Expression::Symbol(sym) = b_name {
                                if sym == "def" {
                                    if found_non_def {
                                        panic!("Internal definitions must appear at the beginning of the body");
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

                    if defs.len() == 0 {
                        let mut fn_ = vec![
                            Expression::Symbol(String::from("fn")),
                            args,
                        ];
                        fn_.extend(bodies);
                        return Expression::List(fn_);
                    }

                    let mut let_bindings = Vec::new();
                    for &(ref n, ref _v) in defs.iter() {
                        let datum_ = vec![n.clone(), Expression::Undefined];
                        let_bindings.push(Expression::List(datum_))
                    }

                    let mut let_ = vec![
                        Expression::Symbol(String::from("let")),
                        Expression::List(let_bindings),
                    ];

                    for (n, v) in defs.into_iter() {
                        let datum_ = vec![
                            Expression::Symbol(String::from("set!")),
                            n,
                            v
                        ];
                        let_.push(Expression::List(datum_));
                    }
                    let_.extend(bodies);

                    let mut fn_ = vec![
                        Expression::Symbol(String::from("fn")),
                        args,
                        Expression::List(let_)
                    ];

                    let res = Expression::List(fn_);
                    return self.expand_macros(res);
                }
            }
            return Expression::List(elems);
        }
        datum
    }

    pub fn preprocess_meaning(&mut self, datum : Expression, env : AEnvRef, tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        match datum {
            Expression::List(mut elems) => {
                let name = elems.remove(0);
                match name {
                    Expression::Symbol(ref s) => {
                        match s.as_ref() {
                            // TODO: Check arity
                            "quote" => self.preprocess_meaning_quotation(elems.remove(0), env, tail),
                            "fn"    => self.preprocess_meaning_abstraction(elems, env, tail),
                            "if"    => self.preprocess_meaning_alternative(elems, env, tail),
                            "do"    => self.preprocess_meaning_sequence(elems, env, tail),
                            "set!"  => self.preprocess_meaning_assignment(elems, env, tail),
                            _       => {
                                // FIXME: Do this without the clone
                                let rules = self.syntax_rules.clone();
                                let rule = rules.get(s).clone();
                                if rule.is_none() {
                                    return self.preprocess_meaning_application(name.clone(), elems, env, tail);
                                }
                                let sr = rule.unwrap().clone();
                                match sr.apply(elems.clone()) {
                                    Some(ex) => self.preprocess_meaning(ex, env, tail),
                                    None => {
                                        panic!("No matching macro pattern for {}",
                                               Expression::List(elems));
                                    }
                                }
                            }
                        }
                    },
                    _ => self.preprocess_meaning_application(name, elems, env, tail)
                }
            },
            Expression::Symbol(symbol) => self.preprocess_meaning_reference(symbol, env, tail),
            other => self.preprocess_meaning_quotation(other, env, tail)
        }
    }

    /**
     * A variable can have three types:
     * - builtin, non overwritable
     * - defined in the global env
     * - defined in some local env
     *
     * Local variables can shadow global & builtin variables,
     * Global variables can shadow builtin variables.
     */
    fn compute_kind(&self, symbol : String, env : AEnvRef) -> VariableKind {
        if let Some(binding) = env.borrow().lookup(&symbol) {
            return VariableKind::Local(binding.0, binding.1);
        }

        if self.global_vars.contains_key(&symbol) {
            return VariableKind::Global(*self.global_vars.get(&symbol).unwrap());
        }

        if self.constant_table.contains_key(&symbol) {
            return VariableKind::Constant(self.constant_table.get(&symbol).unwrap().clone());
        }

        if let Some(builtin) = self.builtins.get_(&symbol) {
            return VariableKind::Builtin(builtin.clone());
        }

        panic!("Undefined variable {}", symbol);
    }

    fn preprocess_meaning_reference(&mut self, symbol : String, env : AEnvRef, _tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        match self.compute_kind(symbol, env) {
            VariableKind::Local(i, j) => {
                if i == 0 {
                    Ok(vec![(Instruction::ShallowArgumentRef(j as u16), None)])
                } else {
                    Ok(vec![(Instruction::DeepArgumentRef(i as u16, j as u16), None)])
                }
            },
            VariableKind::Global(j) => {
                // Checked because the variable might be used before it has been defined
                Ok(vec![(Instruction::CheckedGlobalRef(j as u32), None)])
            },
            VariableKind::Builtin((typ, index, arity)) => {
                // TODO: In the book builtins are handled in a different way,
                // see page 213
                // unimplemented!();
                // FIXME: Currently there is no datum type for LispFns
                let c = self.add_constant(Datum::Builtin(typ, index, arity));
                Ok(vec![(Instruction::Constant(c as u16), None)])
            },
            VariableKind::Constant(i) => {
                Ok(vec![(Instruction::Constant(i as u16), None)])
            }
        }
    }

    fn preprocess_meaning_assignment(&mut self, mut datums : Vec<Expression>, env : AEnvRef, _tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        // TODO: Check arity
        let symbol = datums.remove(0).as_symbol()?;
        let mut res = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;

        match self.compute_kind(symbol.clone(), env) {
            VariableKind::Local(i, j) => {
                if i == 0 {
                    res.push((Instruction::ShallowArgumentSet(j as u16), None));
                } else {
                    res.push((Instruction::DeepArgumentSet(i as u16, j as u16), None));
                }
                Ok(res)
            },
            VariableKind::Global(j) => {
                // Checked because the variable might be used before it has been defined
                res.push((Instruction::GlobalSet(j as u32), None));
                Ok(res)
            },
            VariableKind::Builtin(_fun) => {
                panic!("{} is a reserved name", symbol);
            },
            VariableKind::Constant(_i) => {
                panic!("{} is a constant", symbol);
            }
        }
    }

    fn preprocess_meaning_quotation(&mut self, datum : Expression, _env : AEnvRef, _tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        // TODO: Rewrite once NLL is implemented
        let d : Datum;
        {
            let mut st = self.symbol_table.borrow_mut();
            d = datum.to_datum(&mut st);
        }
        let c = self.add_constant(d);
        Ok(vec![(Instruction::Constant(c as u16), None)])
    }

    fn preprocess_meaning_abstraction(&mut self, mut datums : Vec<Expression>, env : AEnvRef, tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        let names_ = datums.remove(0);
        let mut dotted = false;
        let mut names = Vec::new();

        match names_ {
            Expression::List(elems) => {
                for e in elems {
                    names.push(e.as_symbol().unwrap());
                }
            },
            Expression::DottedList(elems, tail) => {
                for e in elems {
                    names.push(e.as_symbol().unwrap());
                }
                names.push(tail.as_symbol().unwrap());
                dotted = true;
            },
            Expression::Symbol(s) => {
                dotted = true;
                names.push(s);
            },
            Expression::Nil => {},
            _ => panic!("First argument to fn must be a list or a symbol")
        }

        // TODO: Is there even a difference?
        if dotted {
            self.preprocess_meaning_dotted_abstraction(names, datums, env, tail)
        } else {
            self.preprocess_meaning_fix_abstraction(names, datums, env, tail)
        }
    }

    // TODO: Combine these two into one method
    fn preprocess_meaning_fix_abstraction(&mut self, names : Vec<String>, body : Vec<Expression>, env : AEnvRef, _tail : bool)
    -> Result<Vec<LabeledInstruction>, LispErr> {
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
                // TODO: Why 4?
                (Instruction::FixClosure(5, arity as u16), None),
                (Instruction::Jump(label as u32), None)
            ];
            res.extend(body);
            res.push((Instruction::Return, Some(label)));

            Ok(res)
    }
    fn preprocess_meaning_dotted_abstraction(&mut self, names : Vec<String>, body : Vec<Expression>, env : AEnvRef, _tail : bool)
    -> Result<Vec<LabeledInstruction>, LispErr> {
        let arity = names.len();
        let mut env2 = AEnv::new(Some(env));
        env2.extend(names);

        let env2ref = Rc::new(RefCell::new(env2));
        let body = self.preprocess_meaning_sequence(body, env2ref, true)?;

        let label = self.get_uid();

        let mut res = vec![
            // TODO: Why 4?
            (Instruction::DottedClosure(5, arity as u16), None),
            (Instruction::Jump(label as u32), None)
        ];
        res.extend(body);
        res.push((Instruction::Return, Some(label)));

        Ok(res)
    }
    
    /// ```
    /// test
    /// JUMP_FALSE -\
    /// cons        |
    /// JUMP        | -\
    /// alt       <-/  |
    /// ...          <-/
    /// ```
    fn preprocess_meaning_alternative(&mut self, mut datums : Vec<Expression>, env : AEnvRef, tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        let mut test = self.preprocess_meaning(datums.remove(0), env.clone(), false)?;
        let mut cons = self.preprocess_meaning(datums.remove(0), env.clone(), tail)?;

        let mut alt = if datums.len() == 0 {
            let c = self.add_constant(Datum::Nil);
            vec![(Instruction::Constant(c as u16), None)]
        } else {
            self.preprocess_meaning(datums.remove(0), env, tail)?
        };

        let mut last = alt.pop().unwrap();
        let alt_label =
            if let Some(l) = last.1 {
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

    fn preprocess_meaning_sequence(&mut self, datums : Vec<Expression>, env : AEnvRef, tail : bool) -> Result<Vec<LabeledInstruction>, LispErr> {
        let mut res = Vec::new();

        if datums.len() > 0 {
            let last = datums.len() - 1;
            for (i, d) in datums.into_iter().enumerate() {
                let m = self.preprocess_meaning(d, env.clone(), (i == last) && tail)?;
                res.extend(m);
            }
        }

        Ok(res)
    }

    /**
     * Applications can have three forms:
     * - builtin (* 1 2)
     * - closed ((fn (k) (+ k 1)) 2)
     * - regular (user-defined 1 2 3)
     */
    fn preprocess_meaning_application(
        &mut self, fun : Expression,
        datums : Vec<Expression>,
        env : AEnvRef,
        tail : bool
        ) -> Result<Vec<LabeledInstruction>, LispErr> {
        let mut args: Vec<Vec<LabeledInstruction>> = Vec::new();

        for d in datums.into_iter() {
            let res = self.preprocess_meaning(d, env.clone(), false)?;
            args.push(res);
        }

        // TODO: At some point it would be nice to allow predifined variables, too.
        // Once this is implemented, there needs to be a check here.
        let mut res = Vec::new();
        let arity = args.len();

        if let &Expression::Symbol(ref name) = &fun {
            match name.as_ref() {
                "inc" | "dec" | "fst" | "rst" | "not" | "zero?" | "nil?" => {
                    if arity != 1 {
                        panic!("Incorrect arity for monadic VM primitive")
                    }

                    res.extend(args[0].clone());

                    match name.as_ref() {
                        "inc" => res.push((Instruction::Inc, None)),
                        "dec" => res.push((Instruction::Dec, None)),
                        "fst" => res.push((Instruction::Fst, None)),
                        "rst" => res.push((Instruction::Rst, None)),
                        "not" => res.push((Instruction::Not, None)),
                        "zero?" => res.push((Instruction::IsZero, None)),
                        "nil?" => res.push((Instruction::IsNil, None)),
                        other => panic!("Unknown monadic VM primitive {}", other)
                    }
                    return Ok(res)
                },
                "__bin+" | "__bin-" | "__bin*" | "__bin/" |
                "__bin=" | "__bin<" | "__bin>" | "__bin<=" | "__bin>=" |
                "__binequal?" | "cons" | "!=" | "div" | "%" | "vector-ref" => {
                    if arity != 2 {
                        panic!("Incorrect arity for binary VM primitive")
                    }

                    res.extend(args[0].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[1].clone());

                    match name.as_ref() {
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
                        other => panic!("Unknown binary VM primitive {}", other)
                    }
                    return Ok(res)
                },
                "vector-set!" => {
                    if arity != 3 {
                        panic!("Incorrect arity for ternary VM primitive")
                    }

                    res.extend(args[0].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[1].clone());
                    res.push((Instruction::PushValue, None));
                    res.extend(args[2].clone());

                    match name.as_ref() {
                        "vector-set!" => res.push((Instruction::VectorSet, None)),
                        other => panic!("Unknown ternary VM primitive {}", other)
                    }
                    return Ok(res)
                },
                _ => {}
            }
            if let Some(&(ref t, i, ref ar)) = self.builtins.get_(name) {
                ar.check(arity);
                match t {
                    LispFnType::Variadic => {
                        for e in args.into_iter() {
                            res.extend(e);
                            res.push((Instruction::PushValue, None));
                        }
                        res.push((Instruction::CallN(i as u16, arity as u8), None));
                    },
                    LispFnType::Fixed1 => {
                        res.extend(args[0].clone());
                        res.push((Instruction::Call1(i as u16), None));
                    },
                    LispFnType::Fixed2 => {
                        res.extend(args[0].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[1].clone());
                        res.push((Instruction::Call2(i as u16), None));
                    },
                    LispFnType::Fixed3 => {
                        res.extend(args[0].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[1].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[2].clone());
                        res.push((Instruction::Call3(i as u16), None));
                    },
                }

                return Ok(res)
            }
        } 

        if let &Expression::List(ref funl) = &fun {
            if let &Expression::Symbol(ref s) = &funl[0] {
                // If the application is closed
                // there is no need to create a closure and jump to it,
                // just evaluate the arguments in the correct order
                // and continue with the body
                if *s == String::from("fn") {
                    match funl[1].clone() {
                        Expression::List(inner_args) => {
                            let arity = args.len();

                            if arity != inner_args.len() {
                                panic!("Invalid arity");
                            }
                            for e in args.into_iter().rev() {
                                res.extend(e);
                                res.push((Instruction::PushValue, None))
                            }
                            res.push((Instruction::AllocateFillFrame(arity as u32), None));

                            let arg_strs: Vec<String> = inner_args
                                .into_iter()
                                .map(|x| x.as_symbol().unwrap() )
                                .collect();
                            let mut new_env = AEnv::new(Some(env)); 
                            new_env.extend(arg_strs);
                            let new_env_ref = Rc::new(RefCell::new(new_env));

                            let body = self.preprocess_meaning_sequence(funl[2..].to_vec(), new_env_ref, tail)?;

                            res.push((Instruction::ExtendEnv, None));
                            res.extend(body);
                            if !tail {
                                res.push((Instruction::UnlinkEnv, None));
                            }

                            return Ok(res);
                        },
                        Expression::DottedList(inner_args, _tail) => {
                            if args.len() < inner_args.len() {
                                panic!("Invalid arity");
                            }
                            println!("dotted");
                            panic!("Dotted closed applications not supported");
                        },
                        Expression::Symbol(_inner_args) => {
                            println!("single dotted");
                            panic!("Dotted closed applications not supported");
                        },
                        Expression::Nil => {
                            let mut new_env = AEnv::new(Some(env.clone())); 
                            let new_env_ref = Rc::new(RefCell::new(new_env));

                            let body = self.preprocess_meaning_sequence(funl[2..].to_vec(), new_env_ref, tail)?;
                            res.push((Instruction::ExtendEnv, None));
                            res.extend(body);
                            if !tail {
                                res.push((Instruction::UnlinkEnv, None));
                            }
                            return Ok(res);
                        },
                        other => panic!("{} not allowed as function argument", other)
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
        res.push((Instruction::AllocateFillFrame(arity as u32), None));

        res.push((Instruction::PopFunction, None));

        if tail {
            res.push((Instruction::FunctionInvoke(true), None));
        } else {
            res.push((Instruction::PreserveEnv, None));
            res.push((Instruction::FunctionInvoke(false), None));
            res.push((Instruction::RestoreEnv, None));
        }

        Ok(res)
    }
}
