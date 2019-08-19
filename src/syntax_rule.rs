use std::collections::HashMap;

use crate::symbol_table;
use crate::{Symbol, Value};

// Based on R5RS, Section 4.2.3
//
// TODO: Implement SyntaxRuleErrors instead of using unwrap() everywhere
// TODO: Templates like `(name val) ...` don't seem to work
#[derive(Debug, Clone)]
pub struct SyntaxRule {
    name: Symbol,
    literals: Vec<Symbol>,
    rules: Vec<Rule>,
}

#[derive(Debug, Clone)]
pub struct Rule(Pattern, Template);

impl Rule {
    pub fn parse(expr: Value, literals: &Vec<Symbol>) -> Rule {
        let rule = expr.as_list().unwrap();
        let pattern = Pattern::parse(rule[0].clone(), literals);
        let template = Template::parse(rule[1].clone());

        Rule(pattern, template)
    }
}

impl SyntaxRule {
    pub fn parse(name: Symbol, literals: Vec<Value>, rules: Vec<Value>) -> SyntaxRule {
        let literals = literals.iter().map(|l| l.as_symbol().unwrap()).collect();
        let rules = rules
            .into_iter()
            .map(|r| Rule::parse(r, &literals))
            .collect();

        SyntaxRule {
            name,
            literals,
            rules,
        }
    }

    pub fn apply(&self, mut datums: Vec<Value>) -> Option<Value> {
        // Inside the preprocessing step,
        // each function application only has access to its arguments,
        // not its own name,
        // to have the patterns work (somewhat) like specified in R5RS,
        // we need to put it in front again
        datums.insert(0, Value::Symbol(self.name.clone()));
        let datum = Value::make_list_from_vec(datums);

        for &Rule(ref pattern, ref template) in self.rules.iter() {
            let mut bindings: HashMap<Symbol, Value> = HashMap::new();
            if self.matches(pattern, datum.clone(), &mut bindings) {
                return Some(template.apply(&bindings));
            }
        }
        None
    }

    pub fn matches(
        &self,
        pattern: &Pattern,
        datum: Value,
        bindings: &mut HashMap<Symbol, Value>,
    ) -> bool {
        match *pattern {
            Pattern::Ident(name) => {
                if let Value::Symbol(s) = datum {
                    if s == self.name {
                        true
                    } else {
                        bindings.insert(name, datum.clone());
                        true
                    }
                } else {
                    bindings.insert(name, datum.clone());
                    true
                }
            }
            Pattern::Literal(ref name) => {
                if let Value::Symbol(ref s) = datum {
                    s == name
                } else {
                    false
                }
            }
            Pattern::Constant(ref c) => datum == *c,
            Pattern::List(ref patterns) => {
                if datum.is_true_list() {
                    let elems = datum.as_pair().unwrap().collect_list().unwrap();
                    if elems.len() != patterns.len() {
                        return false;
                    }

                    for (s, p) in elems.into_iter().zip(patterns.iter()) {
                        if !self.matches(p, s, bindings) {
                            return false;
                        }
                    }
                    true
                } else if datum == Value::Nil {
                    patterns.is_empty()
                } else {
                    false
                }
            }
            Pattern::ListWithRest(ref patterns, ref rest) => {
                if datum.is_true_list() {
                    let mut elems = datum.as_pair().unwrap().collect_list().unwrap();
                    if elems.len() < patterns.len() {
                        return false;
                    }

                    let remaining = elems.split_off(patterns.len());
                    for (s, p) in elems.into_iter().zip(patterns.iter()) {
                        if !self.matches(p, s, bindings) {
                            return false;
                        }
                    }

                    let mut subbindings = Vec::new();
                    for s in remaining.into_iter() {
                        let mut b = HashMap::new();
                        if !self.matches(rest, s, &mut b) {
                            return false;
                        }
                        subbindings.push(b);
                    }

                    let keys = rest.keys();
                    for k in keys {
                        let mut coll = Vec::new();
                        for subbinding in subbindings.iter() {
                            coll.push(subbinding.get(&k).unwrap().clone());
                        }
                        bindings.insert(k.clone(), Value::make_list_from_vec(coll));
                    }

                    true
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
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
    pub fn parse(expr: Value, literals: &Vec<Symbol>) -> Pattern {
        match expr {
            Value::Nil => Pattern::List(vec![]),
            Value::Symbol(s) => {
                if literals.contains(&s) {
                    Pattern::Literal(s)
                } else {
                    Pattern::Ident(s)
                }
            }
            other => {
                if other.is_true_list() {
                    let mut elems = other.as_pair().unwrap().collect_list().unwrap();
                    let last = elems[elems.len() - 1].clone();
                    if is_ellipsis(last) {
                        elems.pop();
                        let rest = Pattern::parse(elems.pop().unwrap(), literals);
                        Pattern::ListWithRest(
                            elems
                                .into_iter()
                                .map(|e| Pattern::parse(e, literals))
                                .collect(),
                            Box::new(rest),
                        )
                    } else {
                        Pattern::List(
                            elems
                                .into_iter()
                                .map(|e| Pattern::parse(e, literals))
                                .collect(),
                        )
                    }
                } else {
                    Pattern::Constant(other)
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
#[derive(Debug, Clone)]
pub enum Template {
    Ident(Symbol),
    Constant(Value),
    // (<element> ...)
    List(Vec<Element>),
    // // (<element> <element> ... . <template>)
    // Dotted(Vec<Element>, Box<Template>)
}

impl Template {
    pub fn parse(datum: Value) -> Template {
        match datum {
            Value::Symbol(s) => Template::Ident(s),
            Value::Nil => Template::List(vec![]),
            other => {
                // TODO: Handle dotted lists
                if other.is_true_list() {
                    let mut elems = other.as_pair().unwrap().collect_list().unwrap();
                    let mut res = Vec::new();
                    while !elems.is_empty() {
                        let t = Template::parse(elems.remove(0));

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
                    Template::List(res)
                } else {
                    Template::Constant(other)
                }
            }
        }
    }

    pub fn apply(&self, bindings: &HashMap<Symbol, Value>) -> Value {
        match *self {
            Template::Ident(n) => {
                if let Some(d) = bindings.get(&n) {
                    d.clone()
                } else {
                    Value::Symbol(n)
                }
            }
            Template::Constant(ref c) => c.clone(),
            Template::List(ref es) => {
                let mut res = Vec::new();

                for e in es.iter() {
                    match *e {
                        Element::Normal(ref t) => res.push(t.apply(bindings)),
                        Element::Ellipsed(ref t) => match t.apply(bindings) {
                            Value::Nil => {}
                            // TODO: Use lisp error type, later look up identifier
                            // to translate message
                            other => {
                                if other.is_true_list() {
                                    let mut inner =
                                        other.as_pair().unwrap().collect_list().unwrap();
                                    res.append(&mut inner);
                                } else {
                                    panic!(
                                        "macro templates `<identifier> ...`/
                                            only work if binding is list,/
                                            not in {:?}",
                                        other
                                    )
                                }
                            }
                        },
                    }
                }
                Value::make_list_from_vec(res)
            }
        }
    }
}

// <element> is a <template> optionally followed
// by an <ellipsis> ("...")
#[derive(Debug, Clone)]
pub enum Element {
    Normal(Template),
    Ellipsed(Template),
}
