; TODO: (. foo) is broken
; (defn curry (f arg)
;       (fn (. args)
;           (apply f (cons arg args))))
(defn curry2 (f a) (fn (b) (f a b)))


; These are useful for injecting debugging ~> chains
(defn apply-id (f) (fn (x) (f x) x))
(def println-id (apply-id println))

(defsyntax apply () (
  ((apply a b) (__apply a b))))

(defn apply (f args) (__apply f args))

;; (defsyntax eval () (
;;   ((eval a) (__eval a))))

;; (defn eval (f) (__eval f))
