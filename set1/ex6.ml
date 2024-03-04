(*

Analyze the following functions.
How can you use them to check whether the interpreter performs calculations using eager or lazy evaluation?
Justify your answer by showing how the interpreter would calculate the value depending on the evaluation order.
Assume that the evaluation rule of the if expression does not depend on the evaluation order.

*)

let rec f () = f ();;

let test x y = if x = 0 then 0 else y;;

(* ANSWER: *)
test 0 (f())
