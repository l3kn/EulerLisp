pub mod constant_folding;
pub mod error;
mod optimize;
mod preprocessor;

use std::rc::Rc;

use crate::env::{AEnv, AEnvRef};
use crate::instruction::{Instruction, LabeledInstruction, INTEGER_INST_MAX};
use crate::symbol_table::{self, Symbol};
use crate::vm::Context;
use crate::{LispResult, Value};

pub use error::Error;
use preprocessor::{Meaning, Preprocessor, Reference};

#[derive(Debug)]
pub struct Program {
    pub instructions: Vec<LabeledInstruction>,
}

pub struct Compiler {
    current_uid: usize,
    pub context: Rc<Context>,
    preprocessor: Preprocessor,
}

impl Compiler {
    pub fn new(context: Rc<Context>) -> Self {
        Self {
            current_uid: 0,
            preprocessor: Preprocessor::new(context.clone()),
            context,
        }
    }

    /// Generate a unique id, used for jump labels
    fn get_uid(&mut self) -> usize {
        self.current_uid += 1;
        self.current_uid
    }

    pub fn compile(&mut self, datums: Vec<Value>, tail: bool) -> LispResult<Program> {
        let meanings = self.preprocessor.preprocess(datums, tail)?;

        // FIXME: Handle errors
        let mut instructions = Vec::new();
        for d in meanings {
            instructions.extend(self.compile_meaning(d)?);
        }

        Ok(Program {
            instructions: optimize::optimize(instructions),
        })
    }

    pub fn compile_meaning(&mut self, meaning: Meaning) -> LispResult<Vec<LabeledInstruction>> {
        match meaning {
            Meaning::Quotation(datum) => match datum {
                Value::Integer(i) if i >= 0 && i <= INTEGER_INST_MAX => {
                    Ok(vec![(Instruction::Integer(i as u16), None)])
                }
                other => {
                    let constant = self.context.add_anonymous_constant(other);
                    Ok(vec![(Instruction::Constant(constant as u16), None)])
                }
            },
            Meaning::Abstraction(arity, body, dotted, is_macro) => {
                /*
                 * CREATE-CLOSURE -\
                 * GOTO            | -\
                 * body...        <-  |
                 *                  <-/
                 */
                let label = self.get_uid();
                let mut res = vec![];

                if is_macro {
                    if dotted {
                        res.push((Instruction::DottedMacro(arity as u16), None));
                    } else {
                        res.push((Instruction::FixMacro(arity as u16), None));
                    }
                } else {
                    if dotted {
                        res.push((Instruction::DottedClosure(arity as u16), None));
                    } else {
                        res.push((Instruction::FixClosure(arity as u16), None));
                    }
                }
                res.push((Instruction::Jump(label as u32), None));

                res.extend(self.compile_meaning(*body)?);
                res.push((Instruction::Return, Some(label)));

                Ok(res)
            }
            Meaning::Alternative(test, cond, alt) => {
                let mut test = self.compile_meaning(*test)?;
                let mut cons = self.compile_meaning(*cond)?;
                let mut alt = self.compile_meaning(*alt)?;

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
            Meaning::Sequence(datums) => {
                let mut res = Vec::new();
                for d in datums {
                    res.extend(self.compile_meaning(d)?);
                }

                Ok(res)
            }
            Meaning::Assignment(reference, body) => {
                let mut res = self.compile_meaning(*body)?;
                match reference {
                    Reference::Local(i, j) => {
                        if i == 0 {
                            res.push((Instruction::ShallowArgumentSet(j as u16), None));
                        } else {
                            res.push((Instruction::DeepArgumentSet(i as u16, j as u16), None));
                        }
                        Ok(res)
                    }
                    Reference::Global(j) => {
                        // Checked because the variable might be used before it has been defined
                        res.push((Instruction::GlobalSet(j as u16), None));
                        Ok(res)
                    }
                    // NOTE: This is already checked in the preprocessor
                    Reference::Constant(_i) => unimplemented!(),
                }
            }
            // Meaning::MarcoAssignment(reference, body) => {
            //     let mut res = self.compile_meaning(*body)?;
            //     match reference {
            //         Reference::Local(i, j) => {
            //             panic!("Macros are only valid in a global context");
            //             // if i == 0 {
            //             //     res.push((Instruction::ShallowArgumentSet(j as u16), None));
            //             // } else {
            //             //     res.push((Instruction::DeepArgumentSet(i as u16, j as u16), None));
            //             // }
            //             // Ok(res)
            //         }
            //         Reference::Global(j) => {
            //             // Checked because the variable might be used before it has been defined
            //             res.push((Instruction::GlobalSet(j as u16), None));
            //             Ok(res)
            //         }
            //         // NOTE: This is already checked in the preprocessor
            //         Reference::Constant(_i) => unimplemented!(),
            //     }
            // }
            Meaning::Reference(reference) => {
                match reference {
                    Reference::Local(i, j) => {
                        if i == 0 {
                            Ok(vec![(Instruction::ShallowArgumentRef(j as u16), None)])
                        } else {
                            Ok(vec![(
                                Instruction::DeepArgumentRef(i as u16, j as u16),
                                None,
                            )])
                        }
                    }
                    Reference::Global(j) => {
                        // Checked because the variable might be used before it has been defined
                        // TODO: Can we move the check to the preprocessing step?
                        Ok(vec![(Instruction::CheckedGlobalRef(j as u16), None)])
                    }
                    Reference::Constant(i) => Ok(vec![(Instruction::Constant(i as u16), None)]),
                }
            }
            Meaning::BuiltinApplication(name, arg_meanings) => {
                let mut args: Vec<Vec<LabeledInstruction>> = Vec::new();
                for d in arg_meanings {
                    let res = self.compile_meaning(d)?;
                    args.push(res);
                }

                let arity = args.len();

                use symbol_table::*;

                match name {
                    INC | DEC | FST | RST | NOT | IS_ZERO | IS_NIL | CALL_CC | EVAL => {
                        if arity != 1 {
                            return Err(Error::IncorrectPrimitiveArity(name, 1, arity))?;
                        }

                        let mut res = vec![];
                        res.extend(args[0].clone());
                        res.push(self.translate_instruction(name));
                        return Ok(res);
                    }

                    BIN_EQ | BIN_EQUAL | BIN_LT | BIN_LTE | BIN_GT | BIN_GTE | NE | CONS | MOD
                    | VECTOR_REF | APPLY => {
                        if arity != 2 {
                            return Err(Error::IncorrectPrimitiveArity(name, 2, arity))?;
                        }

                        let mut res = vec![];
                        res.extend(args[1].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[0].clone());
                        res.push((Instruction::PopArg1, None));
                        res.push(self.translate_instruction(name));
                        return Ok(res);
                    }
                    ADD | SUB | MUL | DIV | IDIV => {
                        if arity < 2 {
                            // TODO: Allow Min(2) as arity in the error
                            return Err(Error::IncorrectPrimitiveArity(name, 2, arity))?;
                        }

                        let mut res = vec![];
                        // TODO: Variadic VM arithmetic, add arity to inst
                        let instruction = self.translate_instruction(name);
                        for i in (1..args.len()).rev() {
                            res.extend(args[i].clone());
                            res.push((Instruction::PushValue, None));
                        }
                        res.extend(args[0].clone());
                        for _i in 0..(args.len() - 1) {
                            res.push((Instruction::PopArg1, None));
                            res.push(instruction);
                        }

                        return Ok(res);
                    }
                    VECTOR_SET => {
                        if arity != 3 {
                            return Err(Error::IncorrectPrimitiveArity(name, 3, arity))?;
                        }

                        let mut res = vec![];
                        res.extend(args[2].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[1].clone());
                        res.push((Instruction::PushValue, None));
                        res.extend(args[0].clone());
                        res.push((Instruction::PopArg1, None));
                        res.push((Instruction::PopArg2, None));
                        res.push(self.translate_instruction(name));
                        return Ok(res);
                    }
                    // This can't happen
                    other => {
                        unimplemented!();
                    }
                }
            }
            Meaning::ClosedApplicationNil(body, tail) => {
                let mut res = vec![];
                res.push((Instruction::ExtendEnv, None));
                res.extend(self.compile_meaning(*body)?);
                if !tail {
                    res.push((Instruction::UnlinkEnv, None));
                }
                return Ok(res);
            }
            Meaning::ClosedApplication(args, body, tail) => {
                let arity = args.len();
                let mut res = vec![];
                for e in args.into_iter().rev() {
                    res.extend(self.compile_meaning(e)?);
                    res.push((Instruction::PushValue, None))
                }
                res.push((Instruction::AllocateFillFrame(arity as u8), None));
                res.push((Instruction::ExtendEnv, None));
                res.extend(self.compile_meaning(*body)?);
                if !tail {
                    res.push((Instruction::UnlinkEnv, None));
                }
                return Ok(res);
            }
            Meaning::RegularApplication(fun, args, tail) => {
                let mut res = vec![];
                let mfun = self.compile_meaning(*fun)?;
                let arity = args.len();
                res.extend(mfun);
                res.push((Instruction::PushValue, None));

                // Arguments must be stored on the stack first,
                // otherwise they would be overwritten during nested applications
                for e in args.into_iter().rev() {
                    res.extend(self.compile_meaning(e)?);
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
                return Ok(res);
            }
        }
    }

    /// Translate the name of a VM primitive to the corresponding instruction.
    fn translate_instruction(&self, name: Symbol) -> LabeledInstruction {
        use symbol_table::*;
        let instruction = match name {
            INC => Instruction::Inc,
            DEC => Instruction::Dec,
            FST => Instruction::Fst,
            RST => Instruction::Rst,
            NOT => Instruction::Not,
            IS_ZERO => Instruction::IsZero,
            IS_NIL => Instruction::IsNil,
            CALL_CC => Instruction::CallCC,
            EVAL => Instruction::Eval,
            BIN_EQ => Instruction::Eq,
            BIN_EQUAL => Instruction::Equal,
            BIN_LT => Instruction::Lt,
            BIN_LTE => Instruction::Lte,
            BIN_GT => Instruction::Gt,
            BIN_GTE => Instruction::Gte,
            MOD => Instruction::Mod,
            NE => Instruction::Neq,
            CONS => Instruction::Cons,
            VECTOR_REF => Instruction::VectorRef,
            APPLY => Instruction::Apply,
            ADD => Instruction::Add,
            SUB => Instruction::Sub,
            MUL => Instruction::Mul,
            DIV => Instruction::Div,
            IDIV => Instruction::IntDiv,
            VECTOR_SET => Instruction::VectorSet,
            // TODO: Translate symbol name, throw error
            other => panic!("Unknown monadic VM primitive {}", other),
        };
        (instruction, None)
    }
}
