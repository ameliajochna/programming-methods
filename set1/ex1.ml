(*

Analyze the following sequence of expressions.
What result will the interpreter print in response to each of them, assuming that they are calculated in the order in which they are given?
Check your predictions using the interpreter.

*)

10;;

(* int = 10 *)

2 * 3 + 4;;

(* int = 10 *)

2 * (3 + 4);;

(* int = 14 *)

let a = 3;;

(* val a : int = 3 *)

let b = a + 1;;

(* val b : int = 4 *)

if b > a && b < a * b then b else a;;

(* int = 4 *)

2 + (if b > a then b else a);;

(* int = 6 *)

let a = if b * b > b then b else -1 in a * b;;

(* int = 16 *)
