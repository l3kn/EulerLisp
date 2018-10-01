(defn words (s) (string-split " " s))
(defn lines (s) (string-split "\n" s))

(defn string-empty? (s) (zero? (string-length s)))
