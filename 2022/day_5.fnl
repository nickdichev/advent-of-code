(local reader (require :readfile))
(local inspect (require :fennelview))
(local stack (require :stack))

; (fn format-stacks-line [line]
;   (->> (string.gsub line "[%[%]]" "") (pick-values 1)))

(var crate-idx {2 1 6 2 10 3 14 4 18 5 22 6 26 7 30 8 34 9})

(fn find-next-crate [formatted-line]
  (match (string.find formatted-line "%u")
    nil nil
    (idx idx) (values (. crate-idx idx) (string.sub formatted-line idx idx)
                      (->> (string.gsub formatted-line "%u" " " 1)
                           (pick-values 1)))))

(fn parse-stacks-line! [formatted-line stacks]
  (match (find-next-crate formatted-line)
    nil stacks
    (stack-idx crate remaining) (let [crate-stack (. stacks stack-idx)]
                                  ;; we read from top to bottom, so don't stack.push here
                                  (table.insert crate-stack crate)
                                  (parse-stacks-line! remaining stacks))))

(fn init-stacks []
  (fcollect [_ 1 9] (stack.new)))

(fn parse-stacks []
  (var stacks (init-stacks))
  (let [filename :day_5_stack_input]
    (each [line (reader.lines filename)]
      (-> line (parse-stacks-line! stacks))))
  stacks)

(fn collect-matches [str pattern]
  (icollect [val (string.gmatch str pattern)]
    (tonumber val)))

(fn parse-move-line [line] ; (print (inspect (collect-matches line "[%d]+")))
  (let [(num from to) (table.unpack (collect-matches line "[%d]+"))]
    {: num : from : to}))

(fn parse-moves []
  (let [filename :day_5_move_input]
    (icollect [line (reader.lines filename)]
      (parse-move-line line))))

(fn cratemover-9000 [stacks moves]
  (each [_ move (ipairs moves)]
    (let [from_stack (. stacks move.from)
          to_stack (. stacks move.to)]
      (for [f 1 move.num]
        (let [(crate new_from_stack) (stack.pop from_stack)
              new_to_stack (stack.push to_stack crate)]
          (tset stacks move.from new_from_stack)
          (tset stacks move.to new_to_stack)))))
  stacks)

(fn cratemover-9001 [stacks moves]
  (each [_ move (ipairs moves)]
    (let [from_stack (. stacks move.from)
          to_stack (. stacks move.to)
          buffer_stack (stack.new)]
      (for [f 1 move.num]
        (let [(crate new_from_stack) (stack.pop from_stack)]
          (stack.push buffer_stack crate)
          (tset stacks move.from new_from_stack)))
      (for [f 1 move.num]
        (let [(crate _) (stack.pop buffer_stack)]
          (tset stacks move.to (stack.push to_stack crate))))))
  stacks)

(fn make-moves [crane-fn]
  (let [top-crates (icollect [_ sorted-stack (ipairs (crane-fn (parse-stacks)
                                                               (parse-moves)))]
                     (stack.peek sorted-stack))]
    (accumulate [str "" _ top (ipairs top-crates)]
      (.. str top))))

; part 1
(make-moves cratemover-9000)

; part 2
(make-moves cratemover-9001)
