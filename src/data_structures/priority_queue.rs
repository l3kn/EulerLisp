use std::cmp::Ordering;

use ::Datum;

#[derive(Debug, Clone)]
struct PriorityQueueElement {
    element: Datum,
    priority: Datum
}

#[derive(Debug)]
pub struct PriorityQueue {
    array: Vec<PriorityQueueElement>,
    heap_size: usize,
    ordering: Ordering
}

// NOTE: Elements are assumed to be 1-indexed
// ordering is included to allow using this both as a max and min queue
//
// Based on "Introduction to Algorithms", Third Edition, Chapter 6
impl PriorityQueue {
    pub fn new(elements: Vec<Datum>, priorities: Vec<Datum>, ordering: Ordering) -> PriorityQueue {
        if ordering == Ordering::Equal {
            panic!("Only Ordering::Greater and Ordering::Less are valid");
        }

        let array : Vec<PriorityQueueElement> =
            elements.into_iter().zip(priorities.into_iter()).map( |(e, p)|
                PriorityQueueElement { element: e, priority: p }
            ).collect();

        let mut pq = PriorityQueue { heap_size: 0, array, ordering };
        pq.build_max_heap();

        pq
    }

    // Return the maximum/minimum value
    pub fn maximum(&self) -> Datum {
        if self.heap_size < 1 {
            return Datum::Nil
        }

        let max_element = self.array[0].element.clone();
        let max_priority = self.array[0].priority.clone();
        Datum::make_pair(max_element, max_priority)
    }

    // Remove & return the maximum/minimum value
    pub fn pop(&mut self) -> Datum {
        if self.heap_size < 1 {
            return Datum::Nil
        }

        let max_element = self.array[0].element.clone();
        let max_priority = self.array[0].priority.clone();
        let res = Datum::make_pair(max_element, max_priority);

        // To improve performance, the length of the array stays the same,
        // only the heap size is decreased
        self.array[0] = self.array[self.heap_size - 1].clone();
        self.heap_size -= 1;

        // Restore heap property for the new element 1
        self.max_heapify(1);

        res
    }

    pub fn insert(&mut self, element: Datum, priority: Datum) {
        self.heap_size += 1;

        let pqe = PriorityQueueElement { element, priority };
        if self.heap_size <= self.array.len() {
            self.array[self.heap_size - 1] = pqe;
        } else {
            self.array.push(pqe);
        }

        let mut i = self.heap_size;

        // Heap-increase-key
        while i > 1 && self.is_greater(i, self.parent(i)) {
            let parent = self.parent(i);
            self.exchange(i, parent);
            i = parent;
        }
    }

    fn parent(&self, i : usize) -> usize {
        i / 2
    }

    fn left(&self, i : usize) -> usize {
        2 * i
    }

    fn right(&self, i : usize) -> usize {
        2 * i + 1
    }

    fn max_heapify(&mut self, i : usize) {
        let l = self.left(i);
        let r = self.right(i);
        let mut largest;

        if l <= self.heap_size && self.is_greater(l, i) {
            largest = l;
        } else {
            largest = i;
        }
        if r <= self.heap_size && self.is_greater(r, largest) {
            largest = r
        }

        if largest != i {
            self.exchange(i, largest);
            // TODO: TCO
            self.max_heapify(largest);
        }
    }

    fn build_max_heap(&mut self) {
        self.heap_size = self.array.len();
        for i in (1..(self.array.len()/2 + 1)).rev() {
            self.max_heapify(i);
        }
    }

    fn exchange(&mut self, i : usize, j : usize) {
        self.array.swap(i - 1, j - 1);
    }

    fn is_greater(&self, i : usize, j : usize) -> bool {
        self.compare(i, j) == self.ordering
    }

    fn compare(&self, i : usize, j : usize) -> Ordering {
        let a = &self.array[i - 1].priority;
        let b = &self.array[j - 1].priority;

        // TODO: Handle errors, use result type
        a.compare(b).unwrap()
    }
}

