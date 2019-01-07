use crate::Expression::{self, Float, Integer, List, Rational, Symbol};

pub fn fold(datum: Expression) -> Expression {
    if let List(elems) = datum.clone() {
        let mut body: Vec<Expression> = elems.into_iter().map(fold).collect();
        let name = body.remove(0);
        if let Symbol(ref s) = name.clone() {
            match s.as_ref() {
                "neg" => match body[0] {
                    Integer(a) => return Integer(-a),
                    Float(a) => return Float(-a),
                    _ => {}
                },
                "__bin+" => match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a + b),
                    (&Float(a), &Integer(b)) => return Float(a + (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) + b),
                    (&Float(a), &Float(b)) => return Float(a + b),
                    _ => {}
                },
                "__bin-" => match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a - b),
                    (&Float(a), &Integer(b)) => return Float(a - (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) - b),
                    (&Float(a), &Float(b)) => return Float(a - b),
                    _ => {}
                },
                "__bin*" => match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a * b),
                    (&Float(a), &Integer(b)) => return Float(a * (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) * b),
                    (&Float(a), &Float(b)) => return Float(a * b),
                    _ => {}
                },
                "__bin/" => {
                    if body[0].is_numeric() && body[1].is_numeric() {
                        match (&body[0], &body[1]) {
                            (&Integer(a), &Integer(b)) => {
                                return Rational(num::Rational::new(a, b));
                            }
                            (&Float(a), &Integer(b)) => return Float(a / (b as f64)),
                            (&Integer(a), &Float(b)) => return Float((a as f64) / b),
                            (&Float(a), &Float(b)) => return Float(a / b),
                            _ => {}
                        }
                    } else if body[1].is_numeric() {
                        match body[1] {
                            Integer(a) => {
                                return List(vec![
                                    Symbol(String::from("__bin*")),
                                    body[0].clone(),
                                    Rational(num::Rational::new(1, a)),
                                ]);
                            }
                            Float(a) => {
                                let inv = 1.0 / a;
                                return List(vec![
                                    Symbol(String::from("__bin*")),
                                    body[0].clone(),
                                    Float(inv),
                                ]);
                            }
                            _ => {}
                        }
                    }
                }
                "div" => match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a / b),
                    _ => {}
                },
                "pow" => match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a.pow(b as u32)),
                    _ => {}
                },
                "sqrt" => match body[0] {
                    Integer(a) => return Float((a as f64).sqrt()),
                    Float(a) => return Float(a.sqrt()),
                    _ => {}
                },
                _ => {}
            }
        }
    }

    // If no folding is possible, return the original datum
    datum
}
