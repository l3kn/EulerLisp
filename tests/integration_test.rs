extern crate lisp;

use lisp::eval::Evaluator;
use lisp::Datum::*;

#[test]
fn definitions() {
    let mut ev = Evaluator::new();

    ev.eval_str("(def a 1)", 0);
    ev.eval_str("(def b 2)", 0);
    assert_eq!(
        ev.eval_str("a", 0),
        Ok(Number(1))
    );
    assert_eq!(
        ev.eval_str("b", 0),
        Ok(Number(2))
    );
}

#[test]
fn redefinitions() {
    let mut ev = Evaluator::new();

    ev.eval_str("(def a 1)", 0);
    ev.eval_str("(set! a 2)", 0);
    assert_eq!(
        ev.eval_str("a", 0),
        Ok(Number(2))
    );
}

#[test]
fn builtin_read() {
    let mut ev = Evaluator::new();
    assert_eq!(
        ev.eval_str("(read \"1\")", 0),
        Ok(Number(1))
    );
}

#[test]
fn builtin_eval() {
    let mut ev = Evaluator::new();
    assert_eq!(
        ev.eval_str("(eval '(+ 1 2))", 0),
        Ok(Number(3))
    );
    assert_eq!(
        ev.eval_str("(eval (read \"(+ 1 2)\"))", 0),
        Ok(Number(3))
    );
}

#[test]
fn recursion_test() {
    let mut ev = Evaluator::new();
    ev.eval_str("(defn fac (n) (if (= n 0) 1 (* n (fac (- n 1)))))", 0);
    assert_eq!(
        ev.eval_str("(fac 3)", 0),
        Ok(Number(6))
    )
}

#[test]
fn test_set_in_parent_env() {
    let mut ev = Evaluator::new();
    ev.eval_str("(def a 1)", 0);
    ev.eval_str("(defn inc_a () (set! a (inc a)))", 0);
    assert_eq!(ev.eval_str("a", 0), Ok(Number(1)));
    ev.eval_str("(inc_a)", 0);
    assert_eq!(ev.eval_str("a", 0), Ok(Number(2)));
    ev.eval_str("(inc_a)", 0);
    assert_eq!(ev.eval_str("a", 0), Ok(Number(3)));
}

#[test]
fn test_delay_force() {
    let mut ev = Evaluator::new();
    ev.eval_str("(def a 1)", 0);
    ev.eval_str("(def p (delay a))", 0);
    ev.eval_str("(set! a 2)", 0);
    assert_eq!(ev.eval_str("(force p)", 0), Ok(Number(2)));
    ev.eval_str("(set! a 3)", 0);
    assert_eq!(ev.eval_str("(force p)", 0), Ok(Number(2)));
}

#[test]
fn test_delay_force_env() {
    let mut ev = Evaluator::new();

    ev.eval_str("(defn wrap (x) (delay x))", 0);
    assert_eq!(
        ev.eval_str("(force (wrap 1))", 0),
        Ok(Number(1))
    );
}

#[test]
fn test_variadic_fn() {
    let mut ev = Evaluator::new();

    ev.eval_str("(defn arg-len args (length args))", 0);
    assert_eq!(
        ev.eval_str("(arg-len 1 2 3)", 0),
        Ok(Number(3))
    );

    ev.eval_str("(defn arg-fst (a . as) a)", 0);
    assert_eq!(
        ev.eval_str("(arg-fst 1 2 3)", 0),
        Ok(Number(1))
    );

    ev.eval_str("(defn arg-rst (a . as) as)", 0);
    assert_eq!(
        ev.eval_str("(arg-rst 1 2 3)", 0),
        Ok(List(vec!(Number(2), Number(3))))
    );
}
