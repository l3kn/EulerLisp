(defn solution (s) (println "Solution: " s))

(defn integral? (x) (= (floor x) (ceil x)))

(defn triangular (n) (div (* n (+ n 1)) 2))
(defn pentagonal (n) (div (* n (- (* 3 n) 1)) 2))
(defn hexagonal  (n) (* n (- (* 2 n) 1)))
(defn heptagonal (n) (div (* n (- (* 5 n) 3)) 2))
(defn octagonal  (n) (* n (- (* 3 n) 2)))

(defn triangular? (n)
  (let ([solutions (solve-quadratic 1 -1 (- (* 2 n)))])
    (and (not (nil? solutions))
         (integral? (frst solutions)))))

(defn pentagonal? (n)
  (let ([solutions (solve-quadratic 3 -1 (- (* 2 n)))])
    (and (not (nil? solutions))
         (integral? (frst solutions)))))

(defn hexagonal? (n)
  (let ([solutions (solve-quadratic 2 -1 (- n))])
    (and (not (nil? solutions))
         (integral? (frst solutions)))))

(defn input-file-lines (file)
     (~> file
         file-read
         lines
         (reject (fn (line) (equal? "" line)))
         (map string-trim)))

(defn format-digits (prec n)
  (defn round-up (digits acc carry)
    (if (nil? digits)
        (cons acc carry)
        (round-up
          (rst digits)
          (cons
            (% (+ (fst digits) carry) 10)
            acc)
          (div (+ (fst digits) carry) 10))))
  (defn after-decimal (prec n)
    (defn inner (prec n acc)
      (let ([rest (* 10 (- n (floor n)))])
        (if (zero? prec)
          (if (>= rest 5)
            (round-up acc '() 1)
            (cons (reverse acc) 0))
          (inner (dec prec)
                 rest
                 (cons (floor rest)
                       acc)))))
    (inner prec n '()))
  (let ([after (after-decimal prec n)])
    (str
      (+ (floor n) (rst after))
      "."
      (concat (fst after)))))

(defn coprime? (a b) (= 1 (gcd a b)))

;; Floyd's cycle detection algorithm
(defn find-in-cycle (f initial)
  (defn inner (turtoise hare)
        (if (equal? turtoise hare)
            turtoise
            (inner (f turtoise)
                   (f (f hare)))))
      (inner (f initial)
             (f (f initial))))

(defn find-cycle-start (f initial in-cycle)
  (defn inner (turtoise hare mu)
        (if (equal? turtoise hare)
            mu
            (inner (f turtoise) (f hare) (inc mu))))
      (inner initial in-cycle 0))

(defn find-cycle-len (f turtoise)
  (defn inner (turtoise hare len)
        (if (equal? turtoise hare)
            len
            (inner turtoise (f hare) (inc len))))
      (inner turtoise (f turtoise) 1))

(defn find-cycle (f initial)
      (let ((in (find-in-cycle f initial)))
        (cons (find-cycle-start f initial in)
              (find-cycle-len f in))))

(defn palindromic? (lst) (equal? lst (reverse lst)))

(defn primes-below (n)
      (stream-collect (primes-stream (dec n))))

;; $$
;; \sum\limits_{d | n}
;; = \prod\limits_{i = 1}^k \frac{p_i^{e_i + 1} - 1}{p_i - 1}
;; $$
(defn factor-sum (n)
  (- (reduce-product
       (fn (a) (div (dec (pow (fst a) (inc (rst a))))
                   (dec (fst a))))
       (prime-factors n))
     n))

(defn vector-sum (v)
      (sum 0 (dec (vector-length v))
           (fn (i) (vector-ref v i))))

(defn digit-sum (n) (apply + (number->digits n)))
