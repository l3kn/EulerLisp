use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{BindingRef, Symbol, Value};

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
#[derive(Clone, Debug, PartialEq)]
pub struct AEnv {
    bindings: HashMap<Symbol, usize>,
    parent: Option<AEnvRef>,
    counter: usize,
}

pub type AEnvRef = Rc<RefCell<AEnv>>;

impl AEnv {
    pub fn new(parent: Option<AEnvRef>) -> Self {
        AEnv {
            bindings: HashMap::new(),
            parent,
            counter: 0,
        }
    }

    fn lookup_with_depth(&self, key: Symbol, depth: usize) -> Option<BindingRef> {
        if let Some(binding) = self.bindings.get(&key) {
            Some(BindingRef(depth, *binding))
        } else if let Some(ref env_ref) = self.parent {
            env_ref.borrow().lookup_with_depth(key, depth + 1)
        } else {
            None
        }
    }

    pub fn lookup(&self, key: Symbol) -> Option<BindingRef> {
        self.lookup_with_depth(key, 0)
    }

    pub fn insert(&mut self, key: Symbol) -> Option<BindingRef> {
        if self.bindings.contains_key(&key) {
            None
        } else {
            let a = BindingRef(0, self.counter);
            self.bindings.insert(key, self.counter);
            self.counter += 1;
            Some(a)
        }
    }

    pub fn extend(&mut self, keys: Vec<Symbol>) {
        for k in keys {
            self.bindings.insert(k, self.counter);
            self.counter += 1;
        }
    }
}

/// Vector based environments
#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    bindings: Vec<Value>,
    pub parent: Option<EnvRef>,
}

pub type EnvRef = Rc<RefCell<Env>>;

impl Env {
    pub fn new(parent: Option<EnvRef>) -> Self {
        Env {
            bindings: Vec::new(),
            parent,
        }
    }

    pub fn extend(&mut self, values: Vec<Value>) {
        self.bindings.extend(values);
    }

    pub fn shallow_ref(&self, idx: usize) -> Value {
        self.bindings
            .get(idx)
            .expect("Trying to get undefined binding")
            .clone()
    }

    pub fn shallow_set(&mut self, idx: usize, datum: Value) {
        self.bindings[idx] = datum;
    }

    pub fn deep_ref(&self, i: usize, j: usize) -> Value {
        if i == 0 {
            self.bindings
                .get(j)
                .expect("Trying to get undefined binding")
                .clone()
        } else if let Some(ref parent) = self.parent {
            parent.borrow().deep_ref(i - 1, j)
        } else {
            panic!("Trying to get binding with non-zero depth in root env");
        }
    }

    pub fn deep_set(&mut self, i: usize, j: usize, datum: Value) {
        if i == 0 {
            self.bindings[j] = datum;
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().deep_set(i - 1, j, datum);
        } else {
            panic!("Trying to set binding with non-zero depth in root env");
        }
    }
}
