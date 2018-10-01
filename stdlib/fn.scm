; TODO: (. foo) is broken
; (defn curry (f arg)
;       (fn (. args)
;           (apply f (cons arg args))))

; These are useful for injecting debugging ~> chains
(defn apply-id (f) (fn (x) (f x) x))
(def println-id (apply-id println))
