use crate::symbol_table::{BIN_ADD, BIN_DIV, BIN_MUL, BIN_SUB, DIV, NEG, POW, SQRT};
use crate::Value::{self, Float, Integer, Rational, Symbol};

pub fn fold(datum: Value) -> Value {
    if datum.is_true_list() {
        // Due to the is_true_list check, this should not fail
        let elems = datum.as_list().unwrap();
        let mut body: Vec<Value> = elems.into_iter().map(fold).collect();
        let name = body.remove(0);

        if let Symbol(s) = name.clone() {
            // Note: Can't use `match` here as it can't match against constants
            if s == NEG {
                match body[0] {
                    Integer(a) => return Integer(-a),
                    Float(a) => return Float(-a),
                    _ => {}
                }
            } else if s == BIN_ADD {
                match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a + b),
                    (&Float(a), &Integer(b)) => return Float(a + (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) + b),
                    (&Float(a), &Float(b)) => return Float(a + b),
                    _ => {}
                }
            } else if s == BIN_ADD {
                match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a - b),
                    (&Float(a), &Integer(b)) => return Float(a - (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) - b),
                    (&Float(a), &Float(b)) => return Float(a - b),
                    _ => {}
                }
            } else if s == BIN_ADD {
                match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a * b),
                    (&Float(a), &Integer(b)) => return Float(a * (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) * b),
                    (&Float(a), &Float(b)) => return Float(a * b),
                    _ => {}
                }
            } else if s == BIN_ADD {
                match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => {
                        return Rational(num::Rational::new(a, b));
                    }
                    (&Float(a), &Integer(b)) => return Float(a / (b as f64)),
                    (&Integer(a), &Float(b)) => return Float((a as f64) / b),
                    (&Float(a), &Float(b)) => return Float(a / b),
                    (a, &Integer(b)) => {
                        return Value::make_list_from_vec(vec![
                            Symbol(BIN_MUL),
                            a.clone(),
                            Rational(num::Rational::new(1, b)),
                        ]);
                    }
                    (a, &Float(b)) => {
                        let inv = 1.0 / b;
                        return Value::make_list_from_vec(vec![
                            Symbol(BIN_MUL),
                            a.clone(),
                            Float(inv),
                        ]);
                    }
                    _ => {}
                }
            } else if s == DIV {
                match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a / b),
                    _ => {}
                }
            } else if s == POW {
                match (&body[0], &body[1]) {
                    (&Integer(a), &Integer(b)) => return Integer(a.pow(b as u32)),
                    _ => {}
                }
            } else if s == SQRT {
                match body[0] {
                    Integer(a) => return Float((a as f64).sqrt()),
                    Float(a) => return Float(a.sqrt()),
                    _ => {}
                }
            }
        }
    }

    // If no folding is possible, return the original datum
    datum
}
