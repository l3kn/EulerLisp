use ::Expression;
use numbers::Rational;

pub fn fold(datum : Expression) -> Expression {
    match datum.clone() {
        Expression::List(elems) => {
            let mut body : Vec<Expression> = elems.into_iter().map(fold).collect();
            let name = body.remove(0);
            match name.clone() {
                Expression::Symbol(ref s) => {
                    match s.as_ref() {
                        "neg" => {
                            if body[0].is_numeric() {
                                match &body[0] {
                                    &Expression::Integer(a) => {
                                        return Expression::Integer(-a)
                                    },
                                    &Expression::Float(a) => {
                                        return Expression::Float(-a)
                                    },
                                    _ => {}
                                }
                            }
                        },
                        "__bin+" => {
                            if body[0].is_numeric() && body[1].is_numeric() {
                                match (&body[0], &body[1]) {
                                    (&Expression::Integer(a), &Expression::Integer(b)) => {
                                        return Expression::Integer(a + b)
                                    },
                                    (&Expression::Float(a), &Expression::Integer(b)) => {
                                        return Expression::Float(a + (b as f64))
                                    },
                                    (&Expression::Integer(a), &Expression::Float(b)) => {
                                        return Expression::Float((a as f64) + b)
                                    },
                                    (&Expression::Float(a), &Expression::Float(b)) => {
                                        return Expression::Float(a + b)
                                    }
                                    _ => {}
                                }
                            }
                        },
                        "__bin-" => {
                            if body[0].is_numeric() && body[1].is_numeric() {
                                match (&body[0], &body[1]) {
                                    (&Expression::Integer(a), &Expression::Integer(b)) => {
                                        return Expression::Integer(a - b)
                                    },
                                    (&Expression::Float(a), &Expression::Integer(b)) => {
                                        return Expression::Float(a - (b as f64))
                                    },
                                    (&Expression::Integer(a), &Expression::Float(b)) => {
                                        return Expression::Float((a as f64) - b)
                                    },
                                    (&Expression::Float(a), &Expression::Float(b)) => {
                                        return Expression::Float(a - b)
                                    }
                                    _ => {}
                                }
                            }
                        },
                        "__bin*" => {
                            if body[0].is_numeric() && body[1].is_numeric() {
                                match (&body[0], &body[1]) {
                                    (&Expression::Integer(a), &Expression::Integer(b)) => {
                                        return Expression::Integer(a * b)
                                    },
                                    (&Expression::Float(a), &Expression::Integer(b)) => {
                                        return Expression::Float(a * (b as f64))
                                    },
                                    (&Expression::Integer(a), &Expression::Float(b)) => {
                                        return Expression::Float((a as f64) * b)
                                    },
                                    (&Expression::Float(a), &Expression::Float(b)) => {
                                        return Expression::Float(a * b)
                                    }
                                    _ => {}
                                }
                            }
                        },
                        "__bin/" => {
                            if body[0].is_numeric() && body[1].is_numeric() {
                                match (&body[0], &body[1]) {
                                    (&Expression::Integer(a), &Expression::Integer(b)) => {
                                        return Expression::Rational(Rational::new(a, b));
                                    },
                                    (&Expression::Float(a), &Expression::Integer(b)) => {
                                        return Expression::Float(a / (b as f64));
                                    },
                                    (&Expression::Integer(a), &Expression::Float(b)) => {
                                        return Expression::Float((a as f64) / b);
                                    },
                                    (&Expression::Float(a), &Expression::Float(b)) => {
                                        return Expression::Float(a / b);
                                    }
                                    _ => {}
                                }
                            } else if body[1].is_numeric() {
                                match &body[1] {
                                    &Expression::Integer(a) => {
                                        return Expression::List(
                                            vec![
                                            Expression::Symbol(String::from("__bin*")),
                                            body[0].clone(),
                                            Expression::Rational(Rational::new(1, a))
                                            ]
                                            );
                                    },
                                    &Expression::Float(a) => {
                                        let inv = 1.0 / a;
                                        return Expression::List(
                                            vec![
                                            Expression::Symbol(String::from("__bin*")),
                                            body[0].clone(),
                                            Expression::Float(inv)
                                            ]
                                            );
                                    },
                                    _ => {}
                                }
                            }
                        },
                        "div" => {
                            if body[0].is_numeric() && body[1].is_numeric() {
                                match (&body[0], &body[1]) {
                                    (&Expression::Integer(a), &Expression::Integer(b)) => {
                                        return Expression::Integer(a / b);
                                    },
                                    _ => {}
                                }
                            }
                        },
                        "pow" => {
                            if body[0].is_numeric() && body[1].is_numeric() {
                                match (&body[0], &body[1]) {
                                    (&Expression::Integer(a), &Expression::Integer(b)) => {
                                        return Expression::Integer(a.pow(b as u32));
                                    },
                                    _ => {}
                                }
                            }
                        },
                        "sqrt" => {
                            if body[0].is_numeric() {
                                match &body[0] {
                                    &Expression::Integer(a) => {
                                        return Expression::Float((a as f64).sqrt());
                                    },
                                    &Expression::Float(a) => {
                                        return Expression::Float(a.sqrt());
                                    },
                                    _ => {}
                                }
                            }
                        },
                        _ => {}
                    }
                    let mut res = vec![name];
                    res.extend(body);
                    Expression::List(res)
                },
                _ => {
                    let mut res = vec![name];
                    res.extend(body);
                    Expression::List(res)
                }
            }
        },
        other => other
    }
}
