(local reader (require :readfile))
(local inspect (require :fennelview))

(local parsed_input (let [str (reader.file :day_1_input)]
                      (icollect [line (string.gmatch (.. str "\n") "(.-)\n")]
                        line)))

(local calories [])
(var elf_idx 1)
(each [_ value (ipairs parsed_input)]
  (if (not= value "")
      (if (= (. calories elf_idx) nil)
          (tset calories elf_idx [(tonumber value)])
          (table.insert (. calories elf_idx) (tonumber value)))
      (set elf_idx (+ elf_idx 1))))

(fn sum [table_to_sum]
  (var sum 0)
  (each [_ value (ipairs table_to_sum)]
    (set sum (+ sum value)))
  sum)

(each [index calories_list (ipairs calories)]
  (tset calories index (sum calories_list)))

(fn max [table]
  (var max (. table 1))
  (var idx 1)
  (each [index value (ipairs table)]
    (when (> value max)
      (set max value)
      (set idx index)))
  (values max idx))

(fn max_n [input_table num]
  (let [to_sort input_table]
    (table.sort to_sort (fn [left right]
                          (> left right)))
    (table.unpack to_sort 1 num)))

; part 1
(max calories)

; part 2
(sum [(max_n calories 3)])
