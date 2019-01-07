use crate::Datum;
use crate::LispResult;
use crate::LispErr;

pub struct Arena<T: Default> {
    arena: Vec<T>,
    free_list: Vec<usize>,
}

impl<T: Default> Arena<T> {
    pub fn new(size: usize) -> Self {
        let mut arena = Vec::with_capacity(size);
        let mut free_list = Vec::with_capacity(size);

        for i in 0..size {
            arena.push(Default::default());
            free_list.push(i);
        }

        Self { arena, free_list }
    }

    pub fn allocate(&mut self) -> (PairRef, &mut T) {
        if let Some(i) = self.free_list.pop() {
            (i, &mut self.arena[i])
        } else {
            panic!("Arena full");
        }
    }

    pub fn get(&self, i: usize) -> &T {
        &self.arena[i]
    }

    pub fn get_mut(&mut self, i: usize) -> &mut T {
        &mut self.arena[i]
    }
}

use crate::{Vector, Pair, ActivationFrame};
use crate::env::Env;

pub type PairRef = usize;
pub type VectorRef = usize;

pub struct Heap {
    vector_arena: Arena<Vector>,
    pair_arena: Arena<Pair>,
    activation_arena: Arena<ActivationFrame>,
    env_arena: Arena<Env>,
}

impl Heap {
    pub fn new() -> Self {
        // let initial_size = 1_024;
        let initial_size = 1_024 * 8;
        Self {
            vector_arena: Arena::new(initial_size),
            pair_arena: Arena::new(initial_size),
            activation_arena: Arena::new(initial_size),
            env_arena: Arena::new(initial_size),
        }
    }

    pub fn allocate_pair(&mut self) -> (PairRef, &mut Pair) {
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

    pub fn make_pair(&mut self, fst: Datum, rst: Datum) -> Datum {
        let (pair_ref, pair) = self.pair_arena.allocate();
        pair.0 = fst;
        pair.1 = rst;
        Datum::Pair(pair_ref)
    }

    pub fn make_vector(&mut self, elems: Vec<Datum>) -> Datum {
        let (vec_ref, vec) = self.vector_arena.allocate();
        vec.clear();
        vec.extend(elems);
        Datum::Vector(vec_ref)
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


    // Get the list starting with the pair at index `idx`
    pub fn get_pair_list(&self, idx: PairRef) -> LispResult<Vec<Datum>> {
        let mut cur = self.pair_arena.get(idx);
        let mut res = Vec::new();

        loop {
            res.push(cur.0.clone());
            match &cur.1 {
                Datum::Pair(pair_ref) => {
                    cur = self.pair_arena.get(*pair_ref)
                }
                other => {
                    res.push(other.clone());
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

    // pub fn store(&mut self, datum: Datum) -> usize {
    //     let slot = self.free_list.pop().expect("Stack is full");
    //     self.objects[slot] = datum;
    //     slot
    // }

    // pub fn lookup(&self, idx: usize) -> &Datum {
    //     &self.objects[idx]
    // }
}
