(fn new []
  [])

(fn push [stack item]
  (do
    (table.insert stack 1 item)
    stack))

(fn pop [stack]
  (values (table.remove stack 1) stack))

(fn peek [stack]
  (. stack 1))

(-> (new) (push 5) (push 10) (push 3) (pop))

{: new : push : pop : peek}
