# EulerLisp

![](https://projecteuler.net/profile/leonrische.png)

## Code Samples

``` clojure
(~>
  (range~ 1 1000)
  (select~ &(or (divides? 3 &1) (divides? 5 &1)))
  sum~
  (println "Solution: "))
```
## Special Syntax

1. `fst`/`rst` instead of `car`/`cdr`, incl. chained versions `ffst`, ... (`cadr`)
2. `~>` pipeline operator, `(~> 1 (+ 2) (* 3)` = `(* 3 (+ 2 1))`
3. lambda shorthand, `&(+ &1 (* &2 2))` = `(fn (a b) (+ a (* b 2)))`
4. streams (lazy iterators), `range~`, `map~`, `select~`, ...

## Advanced Data Structures

_Note: Some of these could (and should) be implemented in the target language
at some later time._

### Priority Queues

* `(make-priority-queue pairs)`, create a (max-)priority queue from a list of
   (element, priority) pairs
* `(make-min-priority-queue pairs)`, create a (min-)priority queue from a list of
   (element, priority) pairs
* `(priority-queue-insert! pq element priority)`, insert a new element
* `(priority-queue-max pq)`, get the (element, priority) pair with the highest
   (or lowest, if the queue is a min-priority queue) priorty
* `(priority-queue-pop! pq)`, same as `priority-queue-max`, but removes the element

``` clojure
>> (def pq (make-priority-queue (list (cons "foo" 1) (cons "bar" 5) (cons "baz" 7))))
>> (priority-queue-insert! pq "qux" 20)
>> (priority-queue-max pq)
=> ("qux" . 20)
>> (priority-queue-pop! pq)
=> ("qux" . 20)
>> (priority-queue-pop! pq)
=> ("baz" . 7)
>> (priority-queue-pop! pq)
=> ("bar" . 5)
>> (priority-queue-pop! pq)
=> ("foo" . 1)
```

## Favourite Problems

* [169, The different ways a number can be expressed as sum of powers of 2](https://projecteuler.net/problem=169)
* [351, Hexagonal orchards](https://projecteuler.net/problem=351)

## References

### Background

* [The Roots of Lisp](http://paulgraham.com/rootsoflisp.html)

### Algorithms & Data Structures

* [The Art of Computer Programming](https://www-cs-faculty.stanford.edu/~knuth/taocp.html)
* [Introduction to Algorithms](https://mitpress.mit.edu/books/introduction-algorithms)

### Implementation

* [Lisp In Small Pieces](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68CA87D652C82CF849E)

### Specs

* [R5RS](http://www.schemers.org/Documents/Standards/R5RS/)
* [R6RS](http://www.r6rs.org/)
* [R7RS](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/R7RSHomePage.md?fileviewer=file-view-default)
* [The Scheme Programming Language](https://www.scheme.com/tspl4/)
