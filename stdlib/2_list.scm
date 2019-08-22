(def nil '())

(defn ffst (lst) (fst (fst lst)))
(defn frst (lst) (fst (rst lst)))
(defn rfst (lst) (rst (fst lst)))
(defn rrst (lst) (rst (rst lst)))

(defn fffst (lst) (fst (fst (fst lst))))
(defn ffrst (lst) (fst (fst (rst lst))))
(defn frfst (lst) (fst (rst (fst lst))))
(defn frrst (lst) (fst (rst (rst lst))))
(defn rffst (lst) (rst (fst (fst lst))))
(defn rfrst (lst) (rst (fst (rst lst))))
(defn rrfst (lst) (rst (rst (fst lst))))
(defn rrrst (lst) (rst (rst (rst lst))))

(defn ffffst (lst) (fst (fst (fst (fst lst)))))
(defn fffrst (lst) (fst (fst (fst (rst lst)))))
(defn ffrfst (lst) (fst (fst (rst (fst lst)))))
(defn ffrrst (lst) (fst (fst (rst (rst lst)))))
(defn frffst (lst) (fst (rst (fst (fst lst)))))
(defn frfrst (lst) (fst (rst (fst (rst lst)))))
(defn frrfst (lst) (fst (rst (rst (fst lst)))))
(defn frrrst (lst) (fst (rst (rst (rst lst)))))
(defn rfffst (lst) (rst (fst (fst (fst lst)))))
(defn rffrst (lst) (rst (fst (fst (rst lst)))))
(defn rfrfst (lst) (rst (fst (rst (fst lst)))))
(defn rfrrst (lst) (rst (fst (rst (rst lst)))))
(defn rrffst (lst) (rst (rst (fst (fst lst)))))
(defn rrfrst (lst) (rst (rst (fst (rst lst)))))
(defn rrrfst (lst) (rst (rst (rst (fst lst)))))
(defn rrrrst (lst) (rst (rst (rst (rst lst)))))

(defn length (lst)
  (defn inner (lst acc)
    (if (nil? lst)
        acc
        (inner (rst lst) (inc acc))))
  (inner lst 0))

(defn nth (n lst)
  (if (zero? n)
      (fst lst)
      (nth (dec n) (rst lst))))

(defn reverse (lst)
  (defn inner (lst acc)
    (if (nil? lst)
        acc
        (inner (rst lst) (cons (fst lst) acc))))
  (inner lst '()))

(defn range_ (from to step acc)
      (if (< to from) acc
          (range_ from (- to step) step (cons to acc))))
(defn range (from to . r)
  (if (nil? r)
      (range_ from to 1 '())
      (range_ from to (fst r) '())))

(defn none? (pred lst)
  (cond
    [(nil? lst) #t]
    [(pred (fst lst)) #f]
    [else (none? pred (rst lst))]))

(defn any? (pred lst)
  (cond
    [(nil? lst) #f]
    [(pred (fst lst)) #t]
    [else (any? pred (rst lst))]))

(defn all? (pred lst)
  (cond
    [(nil? lst) #t]
    [(pred (fst lst)) (all? pred (rst lst))]
    [else #f]))

(defn member? (e lst) (any? (fn (x) (= x e)) lst))

(defn count (pred lst)
  (defn inner (lst acc)
    (if (nil? lst)
        acc
        (inner (rst lst)
               (if (pred (fst lst)) (inc acc) acc))))
  (inner lst 0))

(defn map (f lst)
  (defn inner (lst acc)
    (if (nil? lst)
        (reverse acc)
        (inner  (rst lst) (cons (f (fst lst)) acc))))
  (inner lst '()))

; (defn map* (f lsts)
;   (defn inner (lsts acc)
;     (if (any? nil? lsts)
;         (reverse acc)
;         (inner (map rst lsts)
;                (cons (apply f (map fst lsts))
;                      acc))))
;   (inner lsts '()))

(defn transpose (lsts)
      (defn inner (lsts acc)
            (if (any? nil? lsts)
                (reverse acc)
                (inner
                  (map rst lsts)
                  (cons (map fst lsts) acc))))
      (inner lsts '()))


(defn reduce (f acc lst)
  (if (nil? lst)
      acc
      (reduce f (f (fst lst) acc) (rst lst))))

(defn append_ (a b)
  (if (nil? a)
      b (append_ (rst a) (cons (fst a) b))))
(defn append (a b) (append_ (reverse a) b))

; TODO: TCO
(defn flatmap (f lst)
      (if (nil? lst)
          '()
          (append (f (fst lst))
                  (flatmap f (rst lst)))))

(defn delete (elem lst)
  (cond ((nil? lst) '())
        ((equal? (fst lst) elem) (rst lst))
        (else (cons (fst lst) (delete elem (rst lst))))))

(defn delete-nth (n lst)
  (if (= n 0)
      (rst lst)
      (cons (fst lst)
            (delete-nth (dec n) (rst lst)))))

(defn select (pred lst)
  (~> lst
      (reduce (fn (a b) (if (pred a) (cons a b) b)) '())
      reverse))

(defn reject (pred lst)
  (~> lst
      (reduce (fn (a b) (unless (pred a) (cons a b) b)) '())
      reverse))

(defn reduce-sum (f lst)
  (reduce (fn (x acc) (+ acc (f x))) 0 lst))

(defn reduce-product (f lst)
  (reduce (fn (x acc) (* acc (f x))) 1 lst))

(defn reduce-max (f lst)
  (if (nil? lst)
      nil
      (reduce (fn (x acc) (max (f x) acc))
              (f (fst lst))
              (rst lst))))
(defn reduce-min (f lst)
  (if (nil? lst)
      nil
      (reduce (fn (x acc) (min (f x) acc))
              (f (fst lst))
              (rst lst))))

(defn max-by (f lst)
  (if (nil? lst)
      '()
      (fst
        (reduce
          (fn (x acc)
              (let ([res (f x)])
                (if (> res (rst acc))
                    (cons x res)
                    acc)))
            (cons (fst lst) (f (fst lst)))
            (rst lst)))))

(defn min-by (f lst)
  (if (nil? lst)
      '()
      (fst
        (reduce
          (fn (x acc)
              (let ([res (f x)])
                (if (< res (rst acc))
                    (cons x res)
                    acc)))
            (cons (fst lst) (f (fst lst)))
            (rst lst)))))

(defn take (n lst)
  (defn inner (n lst acc)
    (if (or (zero? n) (nil? lst))
      (reverse acc)
      (inner (dec n) (rst lst)
            (cons (fst lst) acc))))
  (inner n lst '()))

(defn windows (size lst)
  (defn inner (lst acc)
    (if (< (length lst) size)
        (reverse acc)
        (inner (rst lst)
               (cons (take size lst) acc))))
  (inner lst '()))

(defn chunks (size lst)
  (defn inner (lst n acc-current acc-all)
    (if (nil? lst)
        (if (nil? acc-current)
            (reverse acc-all)
            (reverse (cons (reverse acc-current)
                           acc-all)))
        (if (zero? n)
            (inner lst size
                   '() (cons (reverse acc-current)
                             acc-all))
            (inner (rst lst) (dec n)
                   (cons (fst lst) acc-current)
                   acc-all))))
  (inner lst size '() '()))

(defn last (lst)
  (if (nil? (rst lst))
      (fst lst)
      (last (rst lst))))

(defn flatten (lst) (flatmap (fn (x) x) lst))

(defn each (f lst)
  (reduce (fn (cur acc) (f cur)) '() lst))

(defn each-with-index (f lst)
  (reduce (fn (cur i) (f cur i) (inc i)) 0 lst)
  '())

(defn concat (lst) (join "" lst))

;; Convert a list to a list of pairs `(index . value)`
(defn enumerate (lst)
      (defn inner (lst offset acc)
            (if (nil? lst)
                (reverse acc)
                (inner (rst lst)
                       (inc offset)
                       (cons (cons offset (fst lst)) acc))))
      (inner lst 0 '()))

(defn range-each (from to fun)
      (when (<= from to)
            (fun from)
            (range-each (inc from) to fun)))

(defn reverse-range-each (from to fun)
      (when (<= from to)
            (fun to)
            (reverse-range-each from (dec to) fun)))

(defn range-count (from to pred)
      (defn inner (cur acc)
            (if (> cur to)
                acc
                (if (pred cur)
                    (inner (inc cur) (inc acc))
                    (inner (inc cur) acc))))
      (inner from 0))

(defn range-max (from to f)
      (defn inner (cur best)
            (if (> cur to)
                best
                (inner (inc cur)
                       (max best (f cur)))))
      (inner (inc from)
             (f from)))

(defn range-min (from to f)
      (defn inner (cur best)
            (if (> cur to)
                best
                (inner (inc cur)
                       (min best (f cur)))))
      (inner (inc from)
             (f from)))

(defn vector-init! (v f)
      (range-each 0 (dec (vector-length v))
                  (fn (i)
                      (vector-set! v i (f i)))))

(defn vector-swap! (v i j)
      (let ((tmp (vector-ref v i)))
        (vector-set! v i (vector-ref v j))
        (vector-set! v j tmp)))

(defn vector-add! (v i by)
  (vector-set! v i
     (+ (vector-ref v i) by)))

(defn vector-sub! (v i by)
  (vector-set! v i
     (- (vector-ref v i) by)))

(defn range-first (from to pred)
      (if (> from to)
          '()
          (if (pred from)
              from
              (range-first (inc from) to pred))))

(defn zip_ (lists acc)
    (if (any? nil? lists)
        (reverse acc)
        (zip_ (map rst lists)
                (cons (map fst lists) acc))))
(defn zip lists
      (zip_ lists '()))
