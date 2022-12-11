(local reader (require :readfile))
(local inspect (require :fennelview))

(fn list-ranges []
  (let [filename :day_4_input]
    (icollect [parsed_line (string.gmatch (reader.file filename) "%d*-%d*")]
      (icollect [range-boundary (string.gmatch parsed_line "%d*")]
        (tonumber range-boundary)))))

(fn group-ranges [ranges]
  (fcollect [idx 1 (length ranges) 2]
            [(pick-values 2 (table.unpack ranges idx))]))

(fn fully-contained? [elf_one elf_two]
  (let [elf_one_start (. elf_one 1)
        elf_one_end (. elf_one 2)
        elf_two_start (. elf_two 1)
        elf_two_end (. elf_two 2)]
    (or (and (<= elf_one_start elf_two_start) (>= elf_one_end elf_two_end))
        (and (<= elf_two_start elf_one_start) (>= elf_two_end elf_one_end)))))

(fn has-overlap? [elf_one elf_two]
  (let [elf_one_start (. elf_one 1)
        elf_one_end (. elf_one 2)
        elf_two_start (. elf_two 1)
        elf_two_end (. elf_two 2)]
    (and (>= elf_one_end elf_two_start) (<= elf_one_start elf_two_end))))

(fn sum-with-fn [grouped-ranges overlap-fn]
  (accumulate [sum 0 _ elf-pair (ipairs grouped-ranges)]
    (if (overlap-fn (. elf-pair 1) (. elf-pair 2))
        (+ sum 1)
        sum)))

(local grouped-ranges (-> (list-ranges)
                          (group-ranges)))

; part 1
(sum-with-fn grouped-ranges fully-contained?)

;part 2
(sum-with-fn grouped-ranges has-overlap?)
