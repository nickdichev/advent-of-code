(fn file [filename]
  (var lines "")
  (let [f (assert (io.open filename))]
    (set lines (f:read "*a"))
    (f:close)
  )

  lines
)

(fn lines [filename]
  (io.lines filename)
)

{
  : file
  : lines
}
