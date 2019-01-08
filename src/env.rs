use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{BindingRef, Datum};
use crate::heap::EnvRef;

// This type of environment is only needed
// during the preprocessing phase.
// There we convert each reference to a variable
// to a `BindingRef(depth, binding)`
// where `depth` tells us how many environments
// we need to move up to find a binding
// and `binding` is the index of this binding
// in a list.
//
// Using these two numbers,
// we can repersent environments without HashMaps
// and access to variables should be faster, too

#[derive(Clone, Debug, PartialEq)]
pub struct AEnv {
    bindings: HashMap<String, usize>,
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

    fn lookup_with_depth(&self, key: &str, depth: usize) -> Option<BindingRef> {
        if let Some(binding) = self.bindings.get(key) {
            Some(BindingRef(depth, *binding))
        } else if let Some(ref env_ref) = self.parent {
            env_ref.borrow().lookup_with_depth(key, depth + 1)
        } else {
            None
        }
    }

    pub fn lookup(&self, key: &str) -> Option<BindingRef> {
        self.lookup_with_depth(key, 0)
    }

    pub fn insert(&mut self, key: &str) -> Option<BindingRef> {
        if self.bindings.contains_key(key) {
            None
        } else {
            let a = BindingRef(0, self.counter);
            self.bindings.insert(key.to_string(), self.counter);
            self.counter += 1;
            Some(a)
        }
    }

    pub fn extend(&mut self, keys: Vec<String>) {
        for k in keys {
            self.bindings.insert(k, self.counter);
            self.counter += 1;
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub bindings: Vec<Datum>,
    pub parent: Option<EnvRef>,
}

impl Env {
    pub fn new(parent: Option<EnvRef>) -> Self {
        Env {
            bindings: Vec::new(),
            parent,
        }
    }

    pub fn extend(&mut self, values: Vec<Datum>) {
        self.bindings.extend(values);
    }

    pub fn shallow_ref(&self, idx: usize) -> Datum {
        self.bindings[idx]
    }

    pub fn shallow_set(&mut self, idx: usize, datum: Datum) {
        self.bindings[idx] = datum;
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new(None)
    }
}
