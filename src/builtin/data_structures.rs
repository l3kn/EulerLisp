use std::collections::HashMap;
use std::cmp::Ordering;

use ::Datum;
use ::LispResult;

use ::builtin::*;
use compiler::vm::VM;

use ::data_structures::priority_queue;

fn make_pq(pq: Datum, _vm: &VM) -> LispResult {
    let mut elements = Vec::new();
    let mut priorities = Vec::new();

    if pq != Datum::Nil {
        let content = pq.as_pair()?;
        let kv_pairs = content.collect_list()?;

        for kv_pair in kv_pairs {
            let kv = kv_pair.as_pair()?;
            elements.push(kv.0.clone());
            priorities.push(kv.1.clone());
        }
    }

    let pq = priority_queue::PriorityQueue::new(elements, priorities, Ordering::Greater);
    Ok(Datum::make_priority_queue(pq))
}

fn make_min_pq(pq: Datum, _vm: &VM) -> LispResult {
    let mut elements = Vec::new();
    let mut priorities = Vec::new();

    if pq != Datum::Nil {
        let content = pq.as_pair()?;
        let kv_pairs = content.collect_list()?;

        for kv_pair in kv_pairs {
            let kv = kv_pair.as_pair()?;
            elements.push(kv.0.clone());
            priorities.push(kv.1.clone());
        }
    }

    let pq = priority_queue::PriorityQueue::new(elements, priorities, Ordering::Less);
    Ok(Datum::make_priority_queue(pq))
}

fn pq_maximum(pq: Datum, _vm: &VM) -> LispResult {
    let pq = pq.as_priority_queue()?;
    Ok(pq.maximum())
}

fn pq_pop(pq: Datum, _vm: &VM) -> LispResult {
    let mut pq = pq.as_mut_priority_queue()?;
    Ok(pq.pop())
}

fn pq_insert(pq: Datum, elem: Datum, priority: Datum, _vm: &VM) -> LispResult {
    let mut pq = pq.as_mut_priority_queue()?;
    pq.insert(elem, priority);

    Ok(Datum::Undefined)
}


pub fn load(hm: &mut HashMap<String, LispFn>) {
    register1(hm, "make-priority-queue", make_pq);
    register1(hm, "make-min-priority-queue", make_min_pq);
    register1(hm, "priority-queue-max", pq_maximum);
    register1(hm, "priority-queue-pop!", pq_pop);
    register3(hm, "priority-queue-insert!", pq_insert);
}
