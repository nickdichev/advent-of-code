(local reader (require :readfile))
(local inspect (require :fennelview))

(fn split-input [contents]
  (let [len (string.len contents)
        split_at (// len 2)]
    [(string.sub contents 1 split_at) (string.sub contents (* -1 split_at))]))

(local filename :day_3_input)

(local part1_input (icollect [line (reader.lines filename)]
                     (split-input line)))

(fn chunk-n-lines [lines n]
  (var tab [])
  (var index 1)
  (var count 1)
  (each [line lines]
    (if (= count 1)
        (do
          (tset tab index [line])
          (set count (+ count 1)))
        (= count n)
        (do
          (table.insert (. tab index) line)
          (set count 1)
          (set index (+ index 1)))
        (do
          (table.insert (. tab index) line)
          (set count (+ count 1)))))
  tab)

(local part2_input (chunk-n-lines (reader.lines filename) 3))

(fn rucksack-contents [rucksack]
  (collect [item (string.gmatch rucksack "%a")]
    item
    true))

(fn has-overlap? [left right]
  (let [left-contents (rucksack-contents left)]
    (accumulate [overlap false right-item (string.gmatch right "%a")]
      (if (. left-contents right-item) right-item overlap))))

(fn has-overlap-3? [one two three]
  (let [one-contents (rucksack-contents one)
        two-contents (rucksack-contents two)]
    (accumulate [overlap false three-item (string.gmatch three "%a")]
      (if (and (. one-contents three-item) (. two-contents three-item))
          three-item overlap))))

(fn char-code [char]
  (match (string.byte char)
    (where ascii (and (<= 65 ascii) (>= 90 ascii))) (- ascii 38)
    (where ascii (and (<= 97 ascii) (>= 122 ascii))) (- ascii 96)))

; part 1
(accumulate [sum 0 _ rucksacks (ipairs part1_input)]
  (let [(left right) (table.unpack rucksacks)
        overlap (has-overlap? left right)]
    (if overlap (+ sum (char-code overlap)) sum)))

; part 2
(accumulate [sum 0 _ rucksacks (ipairs part2_input)]
  (let [(one two three) (table.unpack rucksacks)
        overlap (has-overlap-3? one two three)]
    (if overlap (+ sum (char-code overlap)) sum)))
