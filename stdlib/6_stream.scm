(defsyntax delay () (
  ((delay expression) (make-promise (fn () expression)))))

(defn make-promise (proc)
  (let ([result-ready? #f]
        [result #f])
    (fn ()
      (if result-ready?
          result
          (let ([x (proc)])
            (if result-ready?
                result
                (do (set! result-ready? #t)
                    (set! result x)
                    result)))))))

(defn force (obj) (obj))

(defsyntax stream-cons () (
  ((stream-cons head tail) (cons head (delay tail)))))

(defn stream-rst (stream)
  (force (rst stream)))

(defn step-stream (from . by)
      (defn inner (from by)
            (stream-cons from (inner (+ from by) by)))
      (if (nil? by)
          (inner from 1)
          (inner from (fst by))))

(defn stream-enumerate (stream)
      (defn inner (stream i)
            (stream-cons
              (cons (fst stream) i)
              (inner (stream-rst stream) (inc i))))
      (inner stream 0))

(defn stream-select (pred stream)
  (cond
    [(nil? stream) '()]
    [(pred (fst stream))
     (stream-cons
       (fst stream)
       (stream-select pred (stream-rst stream)))]
    [else
     (stream-select pred (stream-rst stream))]))

(defn stream-nth (n stream)
  (cond
    [(nil? stream) '()]
    [(zero? n) (fst stream)]
    [else
     (stream-nth (dec n) (stream-rst stream))]))

(defn range-stream (from to . by)
  (defn inner (from to by)
    (if (> from to)
        '()
        (stream-cons
          from
          (inner (+ from by) to by))))
  (if (nil? by)
      (inner from to 1)
      (inner from to (fst by))))

(defn vector-stream (v)
      (let ((to (vector-length v)))
        (defn inner (i)
              (if (>= i to)
                '()
                (stream-cons
                  (vector-ref v i)
                  (inner (inc i)))))
        (inner 0)))

(defn stream-map (fun stream)
  (if (nil? stream)
      '()
      (stream-cons
        (fun (fst stream))
        (stream-map fun (stream-rst stream)))))

(defn stream-flatmap (fun source-stream)
  (defn inner (current fun source-stream)
    (if (nil? current)
        (if (nil? source-stream)
            '()
            (inner (fun (fst source-stream))
                   fun
                   (stream-rst source-stream)))
        (stream-cons
          (fst current)
          (inner
            (stream-rst current)
            fun
            source-stream))))
  (if (nil? source-stream)
      '()
      (inner '() fun source-stream)))

(defn stream-take-while (pred stream)
  (cond
    [(nil? stream) '()]
    [(pred (fst stream))
     (stream-cons
       (fst stream)
       (stream-take-while
         pred
         (stream-rst stream)))]
    [else '()]))

(defn stream-reduce (fun acc stream)
  (defn inner (stream acc)
    (if (nil? stream)
        acc
        (inner
          (stream-rst stream)
          (fun (fst stream) acc))))
  (inner stream acc))

(defn stream-reduce-sum (f s)
  (stream-reduce (fn (x acc) (+ acc (f x))) 0 s))

(defn stream-sum (stream)
  (stream-reduce + 0 stream))

(defn stream-product (stream)
  (stream-reduce * 1 stream))

(defn stream-count (pred stream)
  (defn inner (acc stream)
    (if (nil? stream)
        acc
        (inner (if (pred (fst stream)) (inc acc) acc)
               (stream-rst stream))))
  (inner 0 stream))

(defn primes-stream (capacity)
  ;; Bitvectors are initialized as false,
  ;; to save time, composite numbers are marked as true
  ;; and primes as false.
  ;;
  ;; Tricks to improve performance:
  ;;
  ;; 1. Only store odd numbers
  ;; 2. Make alternating steps of 2 and 4
  ;;    to skip multiples of 3 `(bitwise-xor step 3)`.
  ;;    this is only valid for primes > 5,
  ;;    so the 2, 3 and 5 must be included manually
  (def cap2 (inc (div capacity 2)))
  (def sieve (make-bitvector cap2))
  (defn remove-multiples (n multiple)
    (when (<= multiple capacity)
      (bitvector-set! sieve (div multiple 2))
      (remove-multiples n (+ multiple n n))))
  (defn init-sieve (cur)
    (if (<= cur capacity)
      (if (bitvector-get sieve (div cur 2))
          (init-sieve (+ 2 cur))
          (do
            (remove-multiples cur (* 3 cur))
            (init-sieve (+ 2 cur))))))
  (defn inner (cur step)
    (if (>= cur cap2)
        '()
        (if (bitvector-get sieve cur)
            (inner (+ cur step) (bitwise-xor step 3))
            (stream-cons
              (inc (* 2 cur))
              (inner (+ cur step) (bitwise-xor step 3))))))
  (cond
    ((< capacity 2) '())
    ((< capacity 3) (stream-cons 2 '()))
    ((< capacity 5) (stream-cons 3 (stream-cons 2 '())))
    (else (init-sieve 3)
          (stream-cons 2
            (stream-cons 3
              (stream-cons 5
                 (inner 3 2)))))))

(defn stream-take (n stream)
  (defn inner (n stream acc)
    ; Stop on 1 to avoid forcing elements that are not needed with stream-rst
    (if (or (= n 1) (nil? stream))
        (reverse (cons (fst stream) acc))
        (inner
          (dec n)
          (stream-rst stream)
          (cons (fst stream) acc))))
  (if (zero? n)
      '()
      (inner n stream '())))

(defn stream-each (f stream)
      (when (not (nil? stream))
            (f (fst stream))
            (stream-each f (stream-rst stream))))

(defn stream-print (stream)
      (stream-each println stream))

(defn stream-max-by (f stream)
  (if (nil? stream)
      '()
      (fst
        (stream-reduce
          (fn (e acc)
              (let ([res (f e)])
                (if (> res (rst acc))
                    (cons e res)
                    acc)))
            (cons (fst stream) (f (fst stream)))
            (stream-rst stream)))))

(defn stream-min-by (f stream)
  (if (nil? stream)
      '()
      (fst
        (stream-reduce
          (fn (e acc)
              (let ([res (f e)])
                (if (< res (rst acc))
                    (cons e res)
                    acc)))
            (cons (fst stream) (f (fst stream)))
            (stream-rst stream)))))

(defn stream-max-of (f stream)
      (if (nil? stream)
        '()
        (stream-reduce
          (fn (e acc)
              (max (f e) acc))
          (f (fst stream))
          (stream-rst stream))))

(defn stream-min-of (f stream)
      (if (nil? stream)
        '()
        (stream-reduce
          (fn (e acc)
              (min (f e) acc))
          (f (fst stream))
          (stream-rst stream))))

(defn stream-max (stream)
  (if (nil? stream)
      '()
      (stream-reduce max (fst stream) (stream-rst stream))))

(defn stream-min (stream)
  (if (nil? stream)
      '()
      (stream-reduce min (fst stream) (stream-rst stream))))

(defn permutations-stream (v)
    (let ([c (make-vector (vector-length v) 0)]
          [i 0]
          [n (vector-length v)]
          [cur (vector-copy v)])
      (defn calculate-next ()
            (when (< i n)
                  (if (< (vector-ref c i) i)
                      (do
                        (if (even? i)
                            (vector-swap! cur 0 i)
                            (vector-swap! cur (vector-ref c i) i))
                        (vector-set! c i (inc (vector-ref c i)))
                        (set! i 0))
                      (do
                        (vector-set! c i 0)
                        (set! i (inc i))
                        (calculate-next)))))
      (defn next ()
            (if (= i n)
                '()
                (let ((ret (vector-copy cur)))
                     (calculate-next)
                     (stream-cons ret (next))
                     )))
      (next)))

(defn stream-length (s)
      (stream-reduce (fn (c acc) (inc acc)) 0 s))

(defn stream-collect (stream)
  (reverse (stream-reduce cons '() stream)))
