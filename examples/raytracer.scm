; Times:
;  * 19.1.18 18.24s
;  * 20.1.18 13.38s, switch to activation frames
;  * 11.2.18 12.03s, switch to real pairs
;  * 26.5.18  7.21s, compiler, first working version
;  * 10.6.18  4.62s, improved VM
;  * 16.6.18  3.88s

(defn vec+ (va vb)
      (list (+ (fst va) (fst vb))
                (+ (frst va) (frst vb))
                (+ (frrst va) (frrst vb))))

(defn vec- (va vb)
      (list (- (fst va) (fst vb))
                (- (frst va) (frst vb))
                (- (frrst va) (frrst vb))))

(defn vec* (va s)
      (list (* (fst va) s)
                (* (frst va) s)
                (* (frrst va) s)))

(defn vec-dot (va vb)
      (+ (* (fst va) (fst vb))
         (* (frst va) (frst vb))
         (* (frrst va) (frrst vb))))

(defn make-ray (o d) (cons o d))

(defn make-sphere ()
      (fn (ray)
          (let ([d (rst ray)]
                [o (fst ray)])
              (let ([solutions (solve-quadratic
                        (vec-dot d d)
                        (* 2 (vec-dot d o))
                        (- (vec-dot o o) 1))])
                (if (not (nil? solutions))
                  (list
                    (fst solutions)
                    (vec+ o (vec* d (fst solutions)))))))))

(def s1 (make-sphere))

(defn loop (obj x y)
  (cond
    [(>= y 400) 'done]
    [(>= x 800) (loop obj 0 (inc y))]
    [else
      (let ([x_ (/ (- x 400) 100)]
            [y_ (/ (- y 200) 100)])
        (let ([hit (obj
                    (make-ray (list x_ y_ 10)
                              (list 0 0 -1)))])
          (if (nil? hit)
              (println "0 0 0")
              (println "255 255 255"))
          (loop obj (inc x) y)
      )
    )]))

(println "P3 800 400 255")
(loop s1 0 0)
