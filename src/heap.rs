use Datum;

pub struct Heap {
    objects: Vec<Datum>,
    free_list: Vec<usize>,
}

impl Heap {
    pub fn new() -> Self {
        let size = 10_000;
        let mut free = vec![];
        for i in 0..size {
            free.push(i);
        }
        free.reverse();

        Heap {
            objects: vec![Datum::Undefined; size],
            free_list: free
        }
    }

    pub fn store(&mut self, datum: Datum) -> usize {
        let slot = self.free_list.pop().expect("Stack is full");
        self.objects[slot] = datum;
        slot
    }

    pub fn lookup(&self, idx: usize) -> &Datum {
        &self.objects[idx]
    }
}
