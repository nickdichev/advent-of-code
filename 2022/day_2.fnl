(local reader (require "readfile"))
(local inspect (require "fennelview"))

(local parsed_input (let [str (reader.file "day_2_input")]
  (icollect
    [line (string.gmatch str "%u%s%u")]
    [(string.sub line 1 1) (string.sub line 3 3)]
  )
))

(local opponent-throw
  {
    "A" :rock
    "B" :paper
    "C" :sissors
  }
)

(local my-throw
  {
    "X" :rock
    "Y" :paper
    "Z" :sissors
  }
)

(local points-per-throw
  {
    :rock    1
    :paper   2
    :sissors 3
  }
)

(local points-per-outcome
  {
    :lose 0
    :draw 3
    :win  6
  }
)

(fn round-result [me opponent]
  (match [me opponent]
    [a a]             :draw
    [:rock :sissors]  :win
    [:paper :rock]    :win
    [:sissors :paper] :win
    _                 :lose
  )
)

(fn play-round [line]
  (var points 0)

  (let
    [
     opponent (. opponent-throw (. line 1))
     me (. my-throw (. line 2))
    ]
    (set points (+ points (. points-per-throw me)))
    (set points (+ points (. points-per-outcome (round-result me opponent))))
  )

  points
)

(local desired-outcome
  {
    "X" :lose
    "Y" :draw
    "Z" :win
  }
)

(fn determine-throw [opponent-throw outcome]
  (match [opponent-throw outcome]
    [opponent :draw] opponent
    [:rock :win]     :paper
    [:paper :win]    :sissors
    [:sissors :win]  :rock
    [:rock :lose]    :sissors
    [:paper :lose]   :rock
    [:sissors :lose] :paper
  )
)

(fn play-round-correctly [line]
  (var points 0)

  (let
    [
     opponent (. opponent-throw (. line 1))
     my_outcome (. desired-outcome (. line 2))
     me (determine-throw opponent my_outcome)
    ]
    (set points (+ points (. points-per-throw me)))
    (set points (+ points (. points-per-outcome my_outcome)))
  )

  points
)

; part 1
(accumulate [sum 0 _ input_line (ipairs parsed_input)]
  (+ sum (play-round input_line))
)

; part 2
(accumulate [sum 0 _ input_line (ipairs parsed_input)]
  (+ sum (play-round-correctly input_line))
)
