use std::collections::HashMap;

use crate::symbol_table::{self, Symbol};
use crate::{LispResult, Value};

// Based on R5RS, Section 4.2.3
//
// TODO: Templates like `(name val) ...` don't seem to work
#[derive(Clone)]
pub struct SyntaxRule {
    name: Symbol,
    literals: Vec<Symbol>,
    rules: Vec<Rule>,
}

#[derive(Clone)]
pub struct Rule(Pattern, Template);

impl Rule {
    pub fn parse(expr: Value, literals: &Vec<Symbol>) -> LispResult<Rule> {
        let rule = expr.as_list()?;
        let pattern = Pattern::parse(rule[0].clone(), literals)?;
        let template = Template::parse(rule[1].clone())?;

        Ok(Rule(pattern, template))
    }
}

impl SyntaxRule {
    pub fn parse(name: Symbol, literals: Vec<Value>, rules: Vec<Value>) -> LispResult<SyntaxRule> {
        let literals: LispResult<Vec<Symbol>> = literals.iter().map(|l| l.as_symbol()).collect();
        let literals = literals?;

        let rules: LispResult<Vec<Rule>> = rules
            .into_iter()
            .map(|r| Rule::parse(r, &literals))
            .collect();
        let rules = rules?;

        Ok(SyntaxRule {
            name,
            literals,
            rules,
        })
    }

    pub fn apply(&self, mut datums: Vec<Value>) -> LispResult<Option<Value>> {
        // Inside the preprocessing step,
        // each function application only has access to its arguments,
        // not its own name,
        // to have the patterns work (somewhat) like specified in R5RS,
        // we need to put it in front again
        datums.insert(0, Value::Symbol(self.name.clone()));
        let datum = Value::make_list_from_vec(datums);

        for &Rule(ref pattern, ref template) in self.rules.iter() {
            let mut bindings: HashMap<Symbol, Value> = HashMap::new();
            if self.matches(pattern, datum.clone(), &mut bindings)? {
                return Ok(Some(template.apply(&bindings)?));
            }
        }
        Ok(None)
    }

    pub fn matches(
        &self,
        pattern: &Pattern,
        datum: Value,
        bindings: &mut HashMap<Symbol, Value>,
    ) -> LispResult<bool> {
        match *pattern {
            Pattern::Ident(name) => {
                if let Value::Symbol(s) = datum {
                    if s == self.name {
                        Ok(true)
                    } else {
                        bindings.insert(name, datum.clone());
                        Ok(true)
                    }
                } else {
                    bindings.insert(name, datum.clone());
                    Ok(true)
                }
            }
            Pattern::Literal(ref name) => {
                if let Value::Symbol(ref s) = datum {
                    Ok(s == name)
                } else {
                    Ok(false)
                }
            }
            Pattern::Constant(ref c) => Ok(datum == *c),
            Pattern::List(ref patterns) => {
                if datum.is_true_list() {
                    let elems = datum.as_pair()?.collect_list()?;
                    if elems.len() != patterns.len() {
                        return Ok(false);
                    }

                    for (s, p) in elems.into_iter().zip(patterns.iter()) {
                        if !self.matches(p, s, bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else if datum == Value::Nil {
                    Ok(patterns.is_empty())
                } else {
                    Ok(false)
                }
            }
            Pattern::ListWithRest(ref patterns, ref rest) => {
                if datum.is_true_list() {
                    let mut elems = datum.as_pair()?.collect_list()?;
                    if elems.len() < patterns.len() {
                        return Ok(false);
                    }

                    let remaining = elems.split_off(patterns.len());
                    for (s, p) in elems.into_iter().zip(patterns.iter()) {
                        if !self.matches(p, s, bindings)? {
                            return Ok(false);
                        }
                    }

                    let mut subbindings = Vec::new();
                    for s in remaining.into_iter() {
                        let mut b = HashMap::new();
                        if !self.matches(rest, s, &mut b)? {
                            return Ok(false);
                        }
                        subbindings.push(b);
                    }

                    let keys = rest.keys();
                    for k in keys {
                        let mut coll = Vec::new();
                        for subbinding in subbindings.iter() {
                            coll.push(subbinding.get(&k)?.clone());
                        }
                        bindings.insert(k.clone(), Value::make_list_from_vec(coll));
                    }

                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }
}

#[derive(Clone)]
pub enum Pattern {
    Ident(Symbol),
    Constant(Value),
    Literal(Symbol),
    // (<pattern> ...)
    List(Vec<Pattern>),
    // TODO: (<pattern> <pattern> ... . <pattern>)
    // (<pattern> ... <pattern> <ellipsis>)
    ListWithRest(Vec<Pattern>, Box<Pattern>),
}

fn is_ellipsis(datum: Value) -> bool {
    if let Value::Symbol(s) = datum {
        s == symbol_table::ELLIPSIS
    } else {
        false
    }
}

impl Pattern {
    pub fn parse(expr: Value, literals: &Vec<Symbol>) -> LispResult<Pattern> {
        match expr {
            Value::Nil => Ok(Pattern::List(vec![])),
            Value::Symbol(s) => {
                if literals.contains(&s) {
                    Ok(Pattern::Literal(s))
                } else {
                    Ok(Pattern::Ident(s))
                }
            }
            other => {
                if other.is_true_list() {
                    let mut elems = other.as_pair()?.collect_list()?;
                    let last = elems[elems.len() - 1].clone();
                    if is_ellipsis(last) {
                        elems.pop();
                        let rest = Pattern::parse(elems.pop()?, literals)?;
                        let patterns: LispResult<Vec<Pattern>> = elems
                            .into_iter()
                            .map(|e| Pattern::parse(e, literals))
                            .collect();
                        Ok(Pattern::ListWithRest(patterns?, Box::new(rest)))
                    } else {
                        let patterns: LispResult<Vec<Pattern>> = elems
                            .into_iter()
                            .map(|e| Pattern::parse(e, literals))
                            .collect();
                        Ok(Pattern::List(patterns?))
                    }
                } else {
                    Ok(Pattern::Constant(other))
                }
            }
        }
    }

    pub fn keys(&self) -> Vec<Symbol> {
        match *self {
            Pattern::List(ref elems) => elems.iter().flat_map(|e| e.keys()).collect(),
            Pattern::ListWithRest(ref elems, ref rest) => {
                let mut res: Vec<Symbol> = elems.iter().flat_map(|e| e.keys()).collect();
                res.append(&mut rest.keys());
                res
            }
            Pattern::Ident(key) => vec![key],
            Pattern::Constant(_) => vec![],
            Pattern::Literal(_) => vec![],
        }
    }
}

// TODO: Find out what elements are used for
#[derive(Clone)]
pub enum Template {
    Ident(Symbol),
    Constant(Value),
    // (<element> ...)
    List(Vec<Element>),
    // // (<element> <element> ... . <template>)
    // Dotted(Vec<Element>, Box<Template>)
}

impl Template {
    pub fn parse(datum: Value) -> LispResult<Template> {
        match datum {
            Value::Symbol(s) => Ok(Template::Ident(s)),
            Value::Nil => Ok(Template::List(vec![])),
            other => {
                // TODO: Handle dotted lists
                if other.is_true_list() {
                    let mut elems = other.as_pair()?.collect_list()?;
                    let mut res = Vec::new();
                    while !elems.is_empty() {
                        let t = Template::parse(elems.remove(0))?;

                        if elems.is_empty() {
                            res.push(Element::Normal(t));
                        } else {
                            // "peek" next element
                            let n = elems.remove(0);

                            if is_ellipsis(n.clone()) {
                                res.push(Element::Ellipsed(t));
                            } else {
                                elems.insert(0, n);
                                res.push(Element::Normal(t));
                            }
                        }
                    }
                    Ok(Template::List(res))
                } else {
                    Ok(Template::Constant(other))
                }
            }
        }
    }

    pub fn apply(&self, bindings: &HashMap<Symbol, Value>) -> LispResult<Value> {
        match *self {
            Template::Ident(n) => {
                if let Some(d) = bindings.get(&n) {
                    Ok(d.clone())
                } else {
                    Ok(Value::Symbol(n))
                }
            }
            Template::Constant(ref c) => Ok(c.clone()),
            Template::List(ref es) => {
                let mut res: Vec<Value> = Vec::new();

                for e in es.iter() {
                    match *e {
                        Element::Normal(ref t) => res.push(t.apply(bindings)?),
                        Element::Ellipsed(ref t) => match t.apply(bindings)? {
                            Value::Nil => {}
                            // TODO: Use lisp error type, later look up identifier
                            // to translate message
                            other => {
                                if other.is_true_list() {
                                    let mut inner = other.as_pair()?.collect_list()?;
                                    res.append(&mut inner);
                                } else {
                                    // TODO: Create error type, include `other`
                                    panic!(
                                        "macro templates `<identifier> ...`/
                                            only work if binding is list"
                                    )
                                }
                            }
                        },
                    }
                }
                Ok(Value::make_list_from_vec(res))
            }
        }
    }
}

// <element> is a <template> optionally followed
// by an <ellipsis> ("...")
#[derive(Clone)]
pub enum Element {
    Normal(Template),
    Ellipsed(Template),
}
