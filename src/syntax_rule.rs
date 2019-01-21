use std::collections::HashMap;

use crate::Expression;

// Based on R5RS, Section 4.2.3
//
// TODO: Implement SyntaxRuleErrors instead of using unwrap() everywhere
// TODO: Templates like `(name val) ...` don't seem to work
#[derive(Debug, Clone)]
pub struct SyntaxRule {
    name: String,
    literals: Vec<String>,
    rules: Vec<Rule>,
}

#[derive(Debug, Clone)]
pub struct Rule(Pattern, Template);

impl Rule {
    pub fn parse(expr: Expression, literals: &Vec<String>) -> Rule {
        let rule = expr.as_list().unwrap();
        let pattern = Pattern::parse(rule[0].clone(), literals);
        let template = Template::parse(rule[1].clone());

        Rule(pattern, template)
    }
}

impl SyntaxRule {
    pub fn parse(name: String, literals: Vec<Expression>, rules: Vec<Expression>) -> SyntaxRule {
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

    pub fn apply(&self, mut datums: Vec<Expression>) -> Option<Expression> {
        // Inside the preprocessing step,
        // each function application only has access to its arguments,
        // not its own name,
        // to have the patterns work (somewhat) like specified in R5RS,
        // we need to put it in front again
        datums.insert(0, Expression::Symbol(self.name.clone()));
        let datum = Expression::List(datums);

        for &Rule(ref pattern, ref template) in self.rules.iter() {
            let mut bindings: HashMap<String, Expression> = HashMap::new();
            if self.matches(pattern, datum.clone(), &mut bindings) {
                return Some(template.apply(&bindings));
            }
        }
        None
    }

    pub fn matches(
        &self,
        pattern: &Pattern,
        datum: Expression,
        bindings: &mut HashMap<String, Expression>,
    ) -> bool {
        match *pattern {
            Pattern::Ident(ref name) => {
                if let Expression::Symbol(ref s) = datum {
                    if *s == self.name {
                        true
                    } else {
                        bindings.insert(name.clone(), datum.clone());
                        true
                    }
                } else {
                    bindings.insert(name.clone(), datum.clone());
                    true
                }
            }
            Pattern::Literal(ref name) => {
                if let Expression::Symbol(ref s) = datum {
                    s == name
                } else {
                    false
                }
            }
            Pattern::Constant(ref c) => datum == *c,
            Pattern::List(ref patterns) => match datum {
                Expression::List(elems) => {
                    if elems.len() != patterns.len() {
                        return false;
                    }

                    for (s, p) in elems.into_iter().zip(patterns.iter()) {
                        if !self.matches(p, s, bindings) {
                            return false;
                        }
                    }
                    true
                }
                Expression::Nil => patterns.is_empty(),
                _ => false,
            },
            Pattern::ListWithRest(ref patterns, ref rest) => match datum {
                Expression::List(mut elems) => {
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
                        bindings.insert(k.clone(), Expression::List(coll));
                    }

                    true
                }
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(String),
    Constant(Expression),
    Literal(String),
    // (<pattern> ...)
    List(Vec<Pattern>),
    // TODO: (<pattern> <pattern> ... . <pattern>)
    // (<pattern> ... <pattern> <ellipsis>)
    ListWithRest(Vec<Pattern>, Box<Pattern>),
}

fn is_ellipsis(datum: Expression) -> bool {
    if let Expression::Symbol(s) = datum {
        s == "..."
    } else {
        false
    }
}

impl Pattern {
    pub fn parse(expr: Expression, literals: &Vec<String>) -> Pattern {
        match expr {
            Expression::List(mut elems) => {
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
            }
            Expression::Nil => Pattern::List(vec![]),
            Expression::Symbol(s) => {
                if literals.contains(&s) {
                    Pattern::Literal(s)
                } else {
                    Pattern::Ident(s)
                }
            }
            other => Pattern::Constant(other),
        }
    }

    pub fn keys(&self) -> Vec<String> {
        match *self {
            Pattern::List(ref elems) => elems.iter().flat_map(|e| e.keys()).collect(),
            Pattern::ListWithRest(ref elems, ref rest) => {
                let mut res: Vec<String> = elems.iter().flat_map(|e| e.keys()).collect();
                res.append(&mut rest.keys());
                res
            }
            Pattern::Ident(ref key) => vec![key.clone()],
            Pattern::Constant(_) => vec![],
            Pattern::Literal(_) => vec![],
        }
    }
}

// TODO: Find out what elements are used for
#[derive(Debug, Clone)]
pub enum Template {
    Ident(String),
    Constant(Expression),
    // (<element> ...)
    List(Vec<Element>),
    // // (<element> <element> ... . <template>)
    // Dotted(Vec<Element>, Box<Template>)
}

impl Template {
    pub fn parse(datum: Expression) -> Template {
        match datum {
            Expression::Symbol(s) => Template::Ident(s),
            Expression::List(mut elems) => {
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
            }
            // TODO: Handle dotted lists
            Expression::Nil => Template::List(vec![]),
            other => Template::Constant(other),
        }
    }

    pub fn apply(&self, bindings: &HashMap<String, Expression>) -> Expression {
        match *self {
            Template::Ident(ref n) => {
                if let Some(d) = bindings.get(n) {
                    d.clone()
                } else {
                    Expression::Symbol(n.clone())
                }
            }
            Template::Constant(ref c) => c.clone(),
            Template::List(ref es) => {
                let mut res = Vec::new();

                for e in es.iter() {
                    match *e {
                        Element::Normal(ref t) => res.push(t.apply(bindings)),
                        Element::Ellipsed(ref t) => match t.apply(bindings) {
                            Expression::List(ref mut inner) => res.append(inner),
                            Expression::Nil => {}
                            other => panic!(
                                "macro templates `<identifier> ...`/
                                            only work if binding is list,/
                                            not in {}",
                                other
                            ),
                        },
                    }
                }
                Expression::List(res)
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
