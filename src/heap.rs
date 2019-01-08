use bitvec::*;

use num::BigInt;

use crate::Datum;
use crate::LispResult;
use crate::LispErr;
use crate::{Vector, Pair};
use crate::env::Env;

pub struct Arena<T: Default> {
    arena: Vec<T>,
    free_list: Vec<usize>,
    marks: BitVec,
    free: BitVec,
    pub should_gc: bool,
    name: String,
}

impl<T: Default> Arena<T> {
    pub fn new(size: usize, name: String) -> Self {
        let mut arena = Vec::with_capacity(size);
        let mut free_list = Vec::with_capacity(size);
        let marks = bitvec![BigEndian, u8; false; size];
        let free = bitvec![BigEndian, u8; true; size];

        for i in (0..size).rev() {
            arena.push(Default::default());
            free_list.push(i);
        }

        Self { arena, free_list, marks, free, should_gc: false, name }
    }

    pub fn grow(&mut self) {
        let old_size = self.arena.len();
        // println!("growing {} arena to {}", self.name, old_size + old_size);
        self.arena.reserve(old_size);
        self.free_list.reserve(old_size);
        self.free.reserve(old_size);
        self.marks.reserve(old_size);

        for i in (0..old_size).rev() {
            self.arena.push(Default::default());
            self.free_list.push(old_size + i);
            // TODO: This is not a good solution
            self.marks.push(false);
            self.free.push(true);
        }
    } 

    pub fn allocate(&mut self) -> (PairRef, &mut T) {
        if let Some(i) = self.free_list.pop() {
            // TODO: Better metric
            if self.free_list.len() < 128 {
                self.should_gc = true;
                // println!("{} arena needs GC", self.name);
            }
            self.free.set(i, false);
            (i, &mut self.arena[i])
        } else {
            panic!("{} arena full", self.name);
        }
    }

    pub fn insert(&mut self, elem: T) -> usize {
        if let Some(i) = self.free_list.pop() {
            if self.free_list.len() < 128 {
                self.should_gc = true;
                // println!("{} arena needs GC", self.name);
            }
            self.free.set(i, false);
            self.arena[i] = elem;
            i
        } else {
            panic!("{} arena full", self.name);
        }
    }

    pub fn get(&self, i: usize) -> &T {
        &self.arena[i]
    }

    pub fn get_mut(&mut self, i: usize) -> &mut T {
        &mut self.arena[i]
    }

    // Return true if the object was newly marked
    pub fn mark(&mut self, i: usize) -> bool {
        if self.marks.get(i) {
            false
        } else {
            self.marks.set(i, true);
            true
        }
    }

    pub fn is_marked(&self, i: usize) -> bool {
        self.marks.get(i)
    }

    pub fn sweep(&mut self) {
        // Only sweep when necessary
        // if self.should_gc {
            for i in 0..self.arena.len() {
                // TODO: Overwrite the garbage value?
                if self.marks.get(i) {
                    self.marks.set(i, false);
                } else if !self.free.get(i) {
                    // self.arena[i] = Default::default();
                    self.free.set(i, true);
                    self.free_list.push(i);
                }
            }
            if self.free_list.len() < 256 {
                self.grow();
            }
            self.should_gc = false;
        // }
    }
}

pub type PairRef = usize;
pub type StringRef = usize;
pub type VectorRef = usize;
pub type ActivationFrameRef = usize;
pub type EnvRef = usize;
pub type BignumRef = usize;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum GarbageCollectable {
    Pair(PairRef),
    String(StringRef),
    Vector(VectorRef),
    ActivationFrame(ActivationFrameRef),
    Env(EnvRef),
    Bignum(BignumRef),
}

pub struct Heap {
    vector_arena: Arena<Vector>,
    pair_arena: Arena<Pair>,
    // Activation frames are stored in the vector arena
    // activation_arena: Arena<ActivationFrame>,
    env_arena: Arena<Env>,
    string_arena: Arena<String>,
    bignum_arena: Arena<BigInt>,
    roots: Vec<GarbageCollectable>,
    should_gc: bool,
}

impl Heap {
    pub fn new() -> Self {
        // FIXME, use better constants & growth conditions
        let initial_pair_size = 1_024 * 1_024;
        let initial_env_size = 1_024 * 1_024;
        let initial_size = 1_024;
        Self {
            vector_arena: Arena::new(initial_env_size, "vector".to_string()),
            pair_arena: Arena::new(initial_pair_size, "pair".to_string()),
            env_arena: Arena::new(initial_env_size, "env".to_string()),
            string_arena: Arena::new(initial_size, "string".to_string()),
            bignum_arena: Arena::new(initial_size, "bignum".to_string()),
            roots: Vec::new(),
            should_gc: false,
        }
    }

    pub fn should_gc(&self) -> bool {
        self.should_gc
    }
    
    pub fn root_datum(&mut self, datum: Datum) {
        match datum {
            Datum::Vector(ptr) => {
                self.roots.push(GarbageCollectable::Vector(ptr));
            }
            Datum::Pair(ptr) => {
                self.roots.push(GarbageCollectable::Pair(ptr));
            }
            Datum::ActivationFrame(ptr) => {
                self.roots.push(GarbageCollectable::ActivationFrame(ptr));
            },
            Datum::String(ptr) => {
                self.roots.push(GarbageCollectable::String(ptr));
            }
            Datum::Bignum(ptr) => {
                self.roots.push(GarbageCollectable::Bignum(ptr));
            }
            Datum::Closure(_, _, _, ptr) => {
                self.roots.push(GarbageCollectable::Env(ptr));
            }
            _ => {}
        }
    }

    pub fn root_env(&mut self, env: EnvRef) {
        self.roots.push(GarbageCollectable::Env(env));
    }

    fn root_datum_(&self, new: &mut Vec<GarbageCollectable>, datum: Datum) {
        match datum {
            Datum::Vector(ptr) => new.push(GarbageCollectable::Vector(ptr)),
            Datum::Pair(ptr) => new.push(GarbageCollectable::Pair(ptr)),
            Datum::ActivationFrame(ptr) => new.push(GarbageCollectable::ActivationFrame(ptr)),
            Datum::String(ptr) => new.push(GarbageCollectable::String(ptr)),
            Datum::Bignum(ptr) => new.push(GarbageCollectable::Bignum(ptr)),
            Datum::Closure(_, _, _, ptr) => new.push(GarbageCollectable::Env(ptr)),
            _ => {}
        }
    }

    pub fn mark(&mut self) {
        self.roots.sort();
        self.roots.dedup();
        while !self.roots.is_empty() {
            let mut new = Vec::new();
            match self.roots.pop().unwrap() {
                GarbageCollectable::Pair(ptr) => {
                    if self.pair_arena.mark(ptr) {
                        let pair = self.get_pair(ptr);
                        self.root_datum_(&mut new, pair.0);
                        self.root_datum_(&mut new, pair.1);
                    }
                }
                GarbageCollectable::Vector(ptr) => {
                    if self.vector_arena.mark(ptr) {
                        let vector = self.get_vector(ptr);
                        for d in vector {
                            self.root_datum_(&mut new, *d);
                        }
                    }
                }
                GarbageCollectable::ActivationFrame(ptr) => {
                    if self.vector_arena.mark(ptr) {
                        let frame = self.get_vector(ptr);
                        for d in frame {
                            self.root_datum_(&mut new, *d);
                        }
                    }
                }
                GarbageCollectable::Env(ptr) => {
                    if self.env_arena.mark(ptr) {
                        let env = self.get_env(ptr);
                        for d in &env.bindings {
                            self.root_datum_(&mut new, *d);
                        }
                        if let Some(parent_ptr) = env.parent {
                            new.push(GarbageCollectable::Env(parent_ptr));
                        }
                    }
                }
                GarbageCollectable::Bignum(ptr) => {
                    self.bignum_arena.mark(ptr);
                },
                GarbageCollectable::String(ptr) => {
                    self.string_arena.mark(ptr);
                }
            }

            for n in new {
                let marked =
                    match n {
                        GarbageCollectable::Pair(ptr) => self.pair_arena.is_marked(ptr),
                        GarbageCollectable::Vector(ptr) => self.vector_arena.is_marked(ptr),
                        GarbageCollectable::ActivationFrame(ptr) => self.vector_arena.is_marked(ptr),
                        GarbageCollectable::Env(ptr) => self.env_arena.is_marked(ptr),
                        GarbageCollectable::String(ptr) => self.string_arena.is_marked(ptr),
                        GarbageCollectable::Bignum(ptr) => self.bignum_arena.is_marked(ptr),
                    };
                if !marked {
                    self.roots.push(n);
                    // self.roots.extend(new);
                }
            }
        }
    }

    pub fn sweep(&mut self) {
        self.env_arena.sweep();
        self.pair_arena.sweep();
        self.vector_arena.sweep();
        self.bignum_arena.sweep();
        self.string_arena.sweep();

        self.roots.clear();
        self.should_gc = false;
    }

    pub fn get_string(&self, idx: StringRef) -> &String {
        self.string_arena.get(idx)
    }

    pub fn get_string_mut(&mut self, idx: StringRef) -> &mut String {
        self.string_arena.get_mut(idx)
    }

    pub fn insert_string(&mut self, string: String) -> StringRef {
        self.should_gc |= self.string_arena.should_gc;
        self.string_arena.insert(string)
    }

    pub fn make_string(&mut self, string: String) -> Datum {
        self.should_gc |= self.string_arena.should_gc;
        Datum::String(self.string_arena.insert(string))
    }

    pub fn allocate_pair(&mut self) -> (PairRef, &mut Pair) {
        self.should_gc |= self.pair_arena.should_gc;
        self.pair_arena.allocate()
    }

    pub fn get_pair(&self, idx: PairRef) -> &Pair {
        self.pair_arena.get(idx)
    }

    pub fn get_pair_mut(&mut self, idx: PairRef) -> &mut Pair {
        self.pair_arena.get_mut(idx)
    }

    pub fn get_vector(&self, idx: VectorRef) -> &Vector {
        self.vector_arena.get(idx)
    }

    pub fn get_vector_mut(&mut self, idx: VectorRef) -> &mut Vector {
        self.vector_arena.get_mut(idx)
    }

    pub fn get_activation_frame(&self, idx: VectorRef) -> &Vector {
        self.vector_arena.get(idx)
    }

    pub fn insert_env(&mut self, env: Env) -> EnvRef {
        self.should_gc |= self.env_arena.should_gc;
        self.env_arena.insert(env)
    }

    pub fn get_env(&self, idx: PairRef) -> &Env {
        self.env_arena.get(idx)
    }

    pub fn get_env_mut(&mut self, idx: PairRef) -> &mut Env {
        self.env_arena.get_mut(idx)
    }

    pub fn get_bignum(&self, idx: VectorRef) -> &BigInt {
        self.bignum_arena.get(idx)
    }

    pub fn get_bignum_mut(&mut self, idx: VectorRef) -> &mut BigInt {
        self.bignum_arena.get_mut(idx)
    }

    pub fn make_bignum(&mut self, bignum: BigInt) -> Datum {
        self.should_gc |= self.bignum_arena.should_gc;
        Datum::Bignum(self.bignum_arena.insert(bignum))
    }

    pub fn make_pair(&mut self, fst: Datum, rst: Datum) -> Datum {
        self.should_gc |= self.pair_arena.should_gc;
        let (pair_ref, pair) = self.pair_arena.allocate();
        pair.0 = fst;
        pair.1 = rst;
        Datum::Pair(pair_ref)
    }

    pub fn make_vector(&mut self, elems: Vec<Datum>) -> Datum {
        self.should_gc |= self.vector_arena.should_gc;
        let (vec_ref, vec) = self.vector_arena.allocate();
        vec.clear();
        vec.extend(elems);
        Datum::Vector(vec_ref)
    }

    pub fn make_activation_frame(&mut self, elems: Vec<Datum>) -> Datum {
        self.should_gc |= self.vector_arena.should_gc;
        let (vec_ref, vec) = self.vector_arena.allocate();
        vec.clear();
        vec.extend(elems);
        Datum::ActivationFrame(vec_ref)
    }

    pub fn make_list_from_vec(&mut self, elems: Vec<Datum>) -> Datum {
        let mut res = Datum::Nil;
        for mut next in elems.into_iter().rev() {
            res = self.make_pair(next.take(), res);
        }
        res
    }

    pub fn make_dotted_list_from_vec(&mut self, elems: Vec<Datum>, tail: Datum) -> Datum {
        let mut res = tail;
        for mut next in elems.into_iter().rev() {
            res = self.make_pair(next.take(), res);
        }
        res
    }

    pub fn get_pair_dotted_list(&self, idx: PairRef) -> Vec<Datum> {
        let mut cur = self.pair_arena.get(idx);
        let mut res = Vec::new();

        loop {
            res.push(cur.0);
            match cur.1 {
                Datum::Pair(pair_ref) => {
                    cur = self.pair_arena.get(pair_ref)
                }
                other => {
                    res.push(other);
                    break;
                }
            }
        }

        res
    }

    // Get the list starting with the pair at index `idx`
    pub fn get_pair_list(&self, idx: PairRef) -> LispResult<Vec<Datum>> {
        let mut cur = self.pair_arena.get(idx);
        let mut res = Vec::new();

        loop {
            res.push(cur.0);
            match cur.1 {
                Datum::Pair(pair_ref) => {
                    cur = self.pair_arena.get(pair_ref)
                }
                other => {
                    res.push(other);
                    break;
                }
            }
        }

        let last = res.pop().unwrap();
        if Datum::Nil == last {
            Ok(res)
        } else {
            Err(LispErr::InvalidList)
        }
    }

    // TODO: Is there a better solution than to implement these
    // inside the heap struct?
    pub fn env_deep_ref(&self, idx: EnvRef, i: usize, j: usize) -> Datum {
        let mut env = self.env_arena.get(idx);
        for _ in 0..i {
            if let Some(parent) = env.parent {
                env = self.env_arena.get(parent);
            } else {
                panic!("Trying to get binding with non-zero depth in root env");
            }
        }
        env.shallow_ref(j)
    }

    pub fn env_deep_set(&mut self, idx: EnvRef, i: usize, j: usize, datum: Datum) {
        let mut env = self.env_arena.get_mut(idx);
        for _ in 0..i {
            if let Some(parent) = env.parent {
                env = self.env_arena.get_mut(parent);
            } else {
                panic!("Trying to set binding with non-zero depth in root env");
            }
        }

        env.shallow_set(j, datum);
    }
}
