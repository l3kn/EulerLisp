// use std::fmt;

// use num::Rational;

// use crate::symbol_table::SymbolTable;
// use crate::{Fsize, LispErr, Value};

// impl Expression {
//     pub fn to_datum(self, st: &mut SymbolTable) -> Value {
//         match self {
//             Expression::Bool(v) => Value::Bool(v),
//             Expression::Integer(v) => Value::Integer(v),
//             Expression::Float(v) => Value::Float(v),
//             Expression::Rational(r) => Value::Rational(r),
//             Expression::Char(v) => Value::Char(v),
//             Expression::String(v) => Value::String(v),
//             Expression::Symbol(v) => Value::Symbol(st.insert(&v)),
//             Expression::List(es) => {
//                 let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
//                 Value::make_list_from_vec(ds)
//             }
//             Expression::DottedList(es, tail) => {
//                 let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
//                 Value::make_dotted_list_from_vec(ds, tail.to_datum(st))
//             }
//             Expression::Vector(es) => {
//                 let ds = es.into_iter().map(|e| e.to_datum(st)).collect();
//                 Value::make_vector_from_vec(ds)
//             }
//             Expression::Undefined => Value::Undefined,
//             Expression::Nil => Value::Nil,
//         }
//     }

//     pub fn as_list(&self) -> Result<Vec<Expression>, LispErr> {
//         match self {
//             &Expression::List(ref elems) => Ok(elems.clone()),
//             &Expression::Nil => Ok(Vec::new()),
//             a => panic!("Can't convert {} to a list", a),
//         }
//     }

//     pub fn as_symbol(&self) -> Result<String, LispErr> {
//         match self {
//             &Expression::Symbol(ref s) => Ok(s.clone()),
//             // TODO: Adapt lisp errors
//             other => panic!("Experted {} to be a symbol", other),
//         }
//     }

//     pub fn is_numeric(&self) -> bool {
//         match *self {
//             Expression::Integer(_) => true,
//             Expression::Float(_) => true,
//             _ => false,
//         }
//     }
// }

// impl PartialEq for Expression {
//     fn eq(&self, other: &Expression) -> bool {
//         match (self, other) {
//             (&Expression::Bool(a), &Expression::Bool(b)) => a == b,
//             (&Expression::Char(a), &Expression::Char(b)) => a == b,
//             (&Expression::Symbol(ref a), &Expression::Symbol(ref b)) => a == b,
//             (&Expression::String(ref a), &Expression::String(ref b)) => a == b,
//             (&Expression::Integer(a), &Expression::Integer(b)) => a == b,
//             (&Expression::Float(a), &Expression::Float(b)) => {
//                 // This is pretty bit hacky but better than not allowing floats
//                 // to be used as hash keys.
//                 // This eq is only meant to be used for hashmaps,
//                 // so it's not that bad.
//                 a.to_string() == b.to_string()
//             }
//             (&Expression::List(ref a1), &Expression::List(ref b1)) => a1 == b1,
//             (&Expression::DottedList(ref a1, ref a2), &Expression::DottedList(ref b1, ref b2)) => {
//                 a1 == b1 && a2 == b2
//             }
//             (&Expression::Vector(ref a), &Expression::Vector(ref b)) => a == b,
//             (&Expression::Undefined, &Expression::Undefined) => true,
//             (&Expression::Nil, &Expression::Nil) => true,
//             _ => false,
//         }
//     }
// }
// impl Eq for Expression {}

// impl fmt::Display for Expression {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match *self {
//             Expression::Symbol(ref v) => write!(f, "{}", v),
//             Expression::Bool(true) => write!(f, "#t"),
//             Expression::Bool(false) => write!(f, "#f"),
//             Expression::Char(c) => write!(f, "#\\{}", c),
//             Expression::List(ref elems) => {
//                 let inner: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
//                 write!(f, "({})", inner.join(" "))
//             }
//             Expression::DottedList(ref elems, ref tail) => {
//                 let inner: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
//                 write!(f, "({} . {})", inner.join(" "), tail)
//             }
//             Expression::Vector(ref elems) => {
//                 let inner: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
//                 write!(f, "#({})", inner.join(" "))
//             }
//             Expression::Integer(x) => write!(f, "{}", x),
//             Expression::Rational(ref x) => write!(f, "{}", x),
//             Expression::Float(x) => write!(f, "{}", x),
//             Expression::String(ref s) => write!(f, "\"{}\"", s),
//             Expression::Nil => write!(f, "'()"),
//             Expression::Undefined => write!(f, "undefined"),
//         }
//     }
// }
