(defn inc (n) (+ n 1))
(defn dec (n) (- n 1))

(defn square (x) (* x x))
(defn cube (x) (* x x x))

(defn even? (n) (= 0 (% n 2)))
(defn odd? (n) (= 1 (% n 2)))
(defn divides? (d n) (= 0 (% n d)))

(defn negative? (n) (< n 0))
(defn positive? (n) (> n 0))

;; For small exponents “normal” multiplication is faster
(defn __pow (b e)
  (cond
    [(= e 0) 1]
    [(even? e) (__pow (* b b) (div e 2))]
    [else (* b (__pow b (dec e)))]))

(defsyntax pow () (
  ((pow a 0) 1)
  ((pow a 1) a)
  ((pow a 2)
   (let ((res a))
     (* res res)))
  ((pow a 3)
   (let ((res a))
     (* res res res)))
  ((pow a 4)
   (let ((res a))
     (* res res res res)))
  ((pow a 5)
   (let ((res a))
     (* res res res res res)))
  ((pow a 6)
   (let ((res a))
     (* res res res res res res)))
  ((pow a n)
   (__pow a n))))

(def pow __pow)

(defn fac (n)
  (defn inner (n acc)
    (if (zero? n) acc
        (inner (dec n) (* acc n))))
  (inner n 1))

(defn abs (x)
  (if (< x 0) (- x) x))
(defn isqrt (n)
  (floor (sqrt n)))
(defn icbrt (n)
  (floor (cbrt n)))
(defn square? (n)
  (= (square (isqrt n)) n))

(defn solve-quadratic (a b c)
  (let ([det (- (square b) (* 4 a c))])
    (cond
      [(< det 0) (list)]
      [(= det 0) (list (/ (- b) (* 2 a)))]
      [else
        (let ([r (sqrt det)])
          (list
            (/ (- (- b) r) (* 2 a))
            (/ (+ (- b) r) (* 2 a))))])))

(defn product (from to fun)
  (defn inner (cur acc)
    (if (> cur to)
        acc
        (inner (inc cur)
               (* acc (fun cur)))))
  (inner from 1))

(defn __sum (from to fun)
  (defn inner (cur acc)
    (if (> cur to)
        acc
        (inner (inc cur)
               (+ acc (fun cur)))))
  (inner from 0))

(defsyntax sum ()
  (((sum from to) (- (gauss-sum to) (gauss-sum (dec from))))
   ((sum from to f)
    (__sum from to f))))

(defn __conditional-sum (from to pred fun)
  (defn inner (cur acc)
    (if (> cur to)
        acc
        (inner (inc cur)
               (if (pred cur)
                   (+ acc (fun cur))
                   acc))))
  (inner from 0))

(defn __conditional-sum-id (from to pred)
  (defn inner (cur acc)
    (if (> cur to)
        acc
        (inner (inc cur)
               (if (pred cur)
                   (+ acc cur)
                   acc))))
  (inner from 0))

(defsyntax sum-if ()
  (((sum-if from to pred f)
   (__conditional-sum from to pred f))
   ((sum-if from to pred)
    (__conditional-sum-id from to pred))))

(defn id (x) x)

(defn fac-from (from to)
  (product from to id))

(defn binomial (n r)
  (let ([other (max r (- n r))])
    (div (fac-from (+ other 1) n)
         (fac-from 1 (- n other)))))

; Create a anonymous polynomial function
; with the given coefficients.
; (polynomial x a b c) = cx^2 + bx + a
(defsyntax polynomial () (
  ((polynomial v) 0)
  ((polynomial v a) a)
  ((polynomial v a rest ...)
   (+ a
      (* v (polynomial v rest ...))))))

(defn gauss-sum (n)
  (div (* n (inc n)) 2))
(defn gauss-square-sum (n)
  (div (* n (inc n) (inc (* 2 n))) 6))
(defn gauss-cube-sum (n)
  (div (* (square n) (square (inc n))) 4))
(defn gauss-fourth-sum (n)
  (div (* n (inc n) (inc (* 2 n)) (polynomial n -1 3 3)) 30))

(defconst PI 3.141592653589793)
(defconst E  2.718281828459045)

(defn indicator (pred)
      (fn (e) (if (pred e) 1 0)))

(defn indicator-id (pred)
      (fn (e) (if (pred e) e 0)))

; Signum function
(defn sgn (n)
     (if (> n 0)
         1
         (if (= n 0) 0 -1)))

(defn not (a) (if a #f #t))

(defn trunc (a)
      (if (< a 0)
          (ceil a)
          (floor a)))

(defn _all-combinations (pfs)
      (if (nil? (rst pfs))
          (map (fn (a) (pow (ffst pfs) a))
               (range 0 (rfst pfs)))
          (let ((rest (_all-combinations (rst pfs))))
            (flatmap (fn (e)
                (let ((pe (pow (ffst pfs) e)))
                  (map (fn (a) (* pe a)) rest)))
                     (range 0 (rfst pfs))))))

(defn factors (n)
      (if (= n 1)
          (list 1)
          (_all-combinations (prime-factors n))))

;; There is a slight difference between __remainder__ `(% a n)`
;; and __modulo__ `(mod a n)`,
;; the first one keeps the sign of `a`,
;; the latter is always in the range $0, \ldots, n-1$.
(defn mod (a n)
      (if (< a 0)
          (+ (% a n) n)
          (% a n)))

(defn modsum (from to f m)
  (defn inner (cur acc)
    (if (> cur to)
        acc
        (inner
         (inc cur)
         (% (+ acc (f cur)) m))))
  (inner from 0))
