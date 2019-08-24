use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::symbol_table::Symbol;
use crate::{BindingRef, Value};

/// "Associative" environments
///
/// This type of environment is only needed during the pre-processing
/// phase.  There each reference to a variable to a `BindingRef(depth,
/// binding)` where `depth` tells us how many environments we need to
/// move up to find a binding and `binding` is the index of this
/// binding in a list.
///
/// Using these two numbers,
/// we can repersent environments without HashMaps
/// and access to variables should be faster, too
#[derive(Clone, PartialEq)]
pub struct AEnv {
    bindings: RefCell<HashMap<Symbol, usize>>,
    parent: Option<AEnvRef>,
    // NOTE: There is no real reason to use a Cell here, I'm using one
    // to add variety
    counter: Cell<usize>,
}

pub type AEnvRef = Rc<AEnv>;

impl AEnv {
    pub fn new(parent: Option<AEnvRef>) -> Self {
        AEnv {
            bindings: RefCell::new(HashMap::new()),
            parent,
            counter: Cell::new(0),
        }
    }

    fn lookup_with_depth(&self, key: Symbol, depth: usize) -> Option<BindingRef> {
        if let Some(binding) = self.bindings.borrow().get(&key) {
            Some(BindingRef(depth, *binding))
        } else if let Some(ref env_ref) = self.parent {
            env_ref.lookup_with_depth(key, depth + 1)
        } else {
            None
        }
    }

    pub fn lookup(&self, key: Symbol) -> Option<BindingRef> {
        self.lookup_with_depth(key, 0)
    }

    pub fn insert(&mut self, key: Symbol) -> Option<BindingRef> {
        let mut bindings = self.bindings.borrow_mut();
        if bindings.contains_key(&key) {
            None
        } else {
            let index = self.counter.get();
            let a = BindingRef(0, index);
            bindings.insert(key, index);
            self.counter.set(index + 1);
            Some(a)
        }
    }

    pub fn extend(&mut self, keys: Vec<Symbol>) {
        for k in keys {
            let index = self.counter.get();
            self.bindings.borrow_mut().insert(k, index);
            self.counter.set(index + 1);
        }
    }
}

/// Vector based environments
#[derive(Clone, PartialEq)]
pub struct Env {
    bindings: RefCell<Vec<Value>>,
    pub parent: Option<EnvRef>,
}

pub type EnvRef = Rc<Env>;

impl Env {
    pub fn new(bindings: Vec<Value>, parent: Option<EnvRef>) -> Self {
        Env {
            bindings: RefCell::new(bindings),
            parent,
        }
    }

    pub fn extend(&mut self, values: Vec<Value>) {
        self.bindings.borrow_mut().extend(values);
    }

    pub fn shallow_ref(&self, idx: usize) -> Value {
        self.bindings
            .borrow()
            .get(idx)
            .expect("Trying to get undefined binding")
            .clone()
    }

    pub fn shallow_set(&self, idx: usize, datum: Value) {
        self.bindings.borrow_mut()[idx] = datum;
    }

    pub fn deep_ref(&self, i: usize, j: usize) -> Value {
        if i == 0 {
            self.bindings
                .borrow()
                .get(j)
                .expect("Trying to get undefined binding")
                .clone()
        } else if let Some(ref parent) = self.parent {
            parent.deep_ref(i - 1, j)
        } else {
            panic!("Trying to get binding with non-zero depth in root env");
        }
    }

    pub fn deep_set(&self, i: usize, j: usize, datum: Value) {
        if i == 0 {
            self.bindings.borrow_mut()[j] = datum;
        } else if let Some(ref parent) = self.parent {
            parent.deep_set(i - 1, j, datum);
        } else {
            panic!("Trying to set binding with non-zero depth in root env");
        }
    }
}
