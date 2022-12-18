(local inspect (require :fennelview))
(local reader (require :readfile))

(local input (reader.file :day_6_input))

(fn inc-field [tbl field]
  (tset tbl field (+ 1 (. tbl field)))
  tbl)

(fn make-frequencies-tbl [s]
  (collect [c (string.gmatch s "%a")]
    c
    0))

(fn frequencies [s]
  (accumulate [tbl (make-frequencies-tbl s) c (string.gmatch s "%a")]
    (inc-field tbl c)))

(fn duplicate-char? [s]
  (accumulate [duplicate? false _ frequency (pairs (frequencies s))]
    (if (> frequency 1) true duplicate?)))

(fn find-non-repeats [pattern-length]
  (fcollect [i (+ 1 pattern-length) (length input)]
            (let [s (string.sub input (- i pattern-length) (- i 1))]
              (if (not (duplicate-char? s)) (- i 1)))))

; part 1
(-> (find-non-repeats 4) (. 1))

; part 2
(-> (find-non-repeats 14) (. 1))
