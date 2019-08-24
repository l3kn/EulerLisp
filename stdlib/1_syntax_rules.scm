(defsyntax defn () (
  ((defn name args body ...)
   (def name (fn args body ...)))))

(defsyntax and () (
  ((and) #t)
  ((and test) test)
  ((and test1 test2 ...)
   (if test1 (and test2 ...) #f))))

(defsyntax or () (
  ((or) #f)
  ((or test) test)
  ((or test1 test2 ...)
   (let ((x test1))
   (if x x (or test2 ...))))))

(defsyntax cond (else =>) (
  ((cond (else result1 result2 ...))
   (do result1 result2 ...))
  ((cond (test1 => result))
   (let ((temp test1))
     (if temp (result temp))))
  ((cond (test2 => result) clause1 clause2 ...)
   (let ((temp test2))
     (if temp
       (result temp)
       (cond clause1 clause2 ...))))
  ((cond (test3)) test3)
  ((cond (test4) clause1 clause2 ...)
   (let ((temp test4))
     (if temp
       temp
       (cond clause1 clause2 ...))))
  ((cond (test5 result1 result2 ...))
   (if test5 (do result1 result2 ...)))
  ((cond (test6 result1 result2 ...)
         clause1 clause2 ...)
   (if test6
     (do result1 result2 ...)
     (cond clause1 clause2 ...)))))

; TODO: R5RS has a version that is more complex,
; does it have any features that would be nice to replicate?
(defsyntax case () (
  ((case (key ...) clauses ...)
   (let ((atom-key (key ...)))
     (case atom-key clauses ...)))
  ((case key (else consq)) consq)
  ((case key (atom consq)) (if (= key atom) consq))
  ((case key (atom consq) rest ...)
   (if (equal? key atom)
       consq
       (case key rest ...)))))

(defsyntax let () (
  ((let () body ...) (do body ...))
  ((let ((name val) ...) body1 body2 ...)
   ((fn (name ...) body1 body2 ...) val ...))
  ((let name ((var val) ...)
     body ...)
   (let ((name nil))
     (set! name (lambda (var ...) body ...))
     (name val ...)))))

(defsyntax unpair ()
  ((unpair (a b) ...)
   (list a ...)))

(defsyntax let* () (
  ((let* ()
     body1 body2 ...)
   (let ()
       body1 body2 ...))
  ((let* ((name1 val1) rest ...)
     body1 body2 ...)
   (let ((name1 val1))
     (let* (rest ...)
       body1 body2 ...)))))

(defsyntax when () (
  ((when test body ...)
   (if test (do body ...)))))

(defsyntax unless () (
  ((unless test conseq)
   (if test '() conseq))
  ((unless test conseq alt)
   (if test alt conseq))))

(defsyntax ~> () (
  ((~> first) first)
  ((~> first (second args ...))
   (second args ... first))
  ((~> first second)
   (second first))
  ((~> first (second args ...) rest ...)
   (~> (second args ... first) rest ...))
  ((~> first second rest ...)
   (~> (second first) rest ...))))

;; (defsyntax letrec_generate_temp_names () (
;;   ((letrec_generate_temp_names
;;      ()
;;      (temp1 ...)
;;      ((var1 init1) ...)
;;      body ...)
;;    (let ((var1 nil) ...)
;;      (let ((temp1 init1) ...)
;;        (set! var1 temp1)
;;        ...
;;        body ...)))
;;   ((letrec_generate_temp_names
;;      (x y ...)
;;      (temp ...)
;;      ((var1 init1) ...)
;;      body ...)
;;    (letrec_generate_temp_names
;;      (y ...)
;;      (newtemp temp ...)
;;      ((var1 init1) ...)
;;      body ...))))

;; (defsyntax letrec () (
;;   ((letrec ((var1 init1) ...) body ...)
;;    (letrec_generate_temp_names
;;      (var1 ...)
;;      ()
;;      ((var1 init1) ...)
;;      body ...))))

; (defsyntax for_step () (
;   ((for_step x) x)
;   ((for_step x y) y)))

; (defsyntax for () (
;   ((do ((var init step ...) ...)
;        (test expr ...)
;        command ...)
;    (letrec
;      ((loop
;         (lambda (var ...)
;                 (if test
;                   (do
;                     (if #f #f)
;                     expr ...)
;                   (do
;                     command ...
;                     (loop (for_step var step ...) ...))))))
;      (loop init ...)))))

(defsyntax debug () (
  ((debug var)
   (println 'var ": " var))))

; Function composition
; TODO: Generalize to n arguments
(defsyntax comp ()
    (((comp) (fn (x) x))
     ((comp f) (fn (x) (f x)))
     ((comp g f) (fn (x) (g (f x))))
     ((comp h g f) (fn (x) (h (g (f x)))))
     ((comp i h g f) (fn (x) (i (h (g (f x))))))
     ((comp j i h g f) (fn (x) (j (i (h (g (f x)))))))
     ((comp k j i h g f) (fn (x) (k (j (i (h (g (f x))))))))
     ((comp l k j i h g f) (fn (x) (l (k (j (i (h (g (f x)))))))))))

; (defsyntax list () (
;   ((list) '())
;   ((list a rest ...)
;    (cons a (list rest ...)))))

; (def list __list)

(defsyntax + () (
  ((+) 0)
  ((+ a) a)
  ((+ a 1) (inc a))
  ((+ 1 a) (inc a))
  ((+ a b ...) (__+ a b ...))))

(defsyntax - () (
  ((- a) (__neg a))
  ((- a 1) (dec a))
  ((- a b ...) (__- a b ...))))

(defsyntax * () (
  ((*) 1)
  ((* a) a)
  ((* a 0) 0)
  ((* 0 a) 0)
  ((* a 1) a)
  ((* 1 a) a)
  ((* a b ...) (__* a b ...))))

(defsyntax / () (
  ((/ a b ...) (__/ a b ...))))

(defsyntax = () (
  ((= 0 a) (zero? a))
  ((= a 0) (zero? a))
  ((= a '()) (nil? a))
  ((= '() a) (nil? a))
  ((= a b) (__bin= a b))
  ((= a b c ...) (__var= a b c ...))))
(defsyntax < () (
  ((< a b) (__bin< a b))
  ((< a b c ...) (__var< a b c ...))))
(defsyntax <= () (
  ((<= a b) (__bin<= a b))
  ((<= a b c ...) (__var<= a b c ...))))
(defsyntax > () (
  ((> a b) (__bin> a b))
  ((> a b c ...) (__var> a b c ...))))
(defsyntax >= () (
  ((>= a b) (__bin>= a b))
  ((>= a b c ...) (__var>= a b c ...))))
(defsyntax gcd () (
  ((gcd a b) (__bingcd a b))
  ((gcd a b c ...) (__vargcd a b c ...))))
(defsyntax lcm () (
  ((lcm a b) (__binlcm a b))
  ((lcm a b c ...) (__varlcm a b c ...))))
(defsyntax equal? () (
  ((equal? a b) (__binequal? a b))
  ((equal? a b c ...) (__varequal? a b c ...))))
(defsyntax min () (
  ((min a b) (__binmin a b))
  ((min a b c ...) (__varmin a b c ...))))
(defsyntax max () (
  ((max a b) (__binmax a b))
  ((max a b c ...) (__varmax a b c ...))))
(defsyntax bitwise-and () (
  ((bitwise-and a b) (__binbitwise-and a b))
  ((bitwise-and a b c ...) (__varbitwise-and a b c ...))))
(defsyntax bitwise-or () (
  ((bitwise-or a b) (__binbitwise-or a b))
  ((bitwise-or a b c ...) (__varbitwise-or a b c ...))))
(defsyntax bitwise-xor () (
  ((bitwise-xor a b) (__binbitwise-xor a b))
  ((bitwise-xor a b c ...) (__varbitwise-xor a b c ...))))

; For use in `reduce *` etc
(def + __+)
(def - __-)
(def * __*)
(def / __/)
(def = __var=)
(def < __var<)
(def <= __var<=)
(def > __var>)
(def >= __var>=)
(def equal? __varequal?)
(def gcd __vargcd)
(def lcm __varlcm)
(def min __varmin)
(def max __varmax)
(def bitvector-and __varbitwise-and)
(def bitvector-or __varbitwise-or)
(def bitvector-xor __varbitwise-xor)
