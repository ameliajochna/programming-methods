(*

The composition of the functions f and g is defined (as we remember from the course "Logic for computer scientists") as a function x |-> f(g(x)).
Define a two-argument compose function that combines the (one-argument) functions passed to it as arguments.
Using the substitution model, trace the execution of the expressions:

compose square inc 5
compose inc square 5

We assume that the square function calculates the square of its argument, and inc – the argument value increased by 1.

*)


let square x = x*x;;
let inc x = x+1;;

let compose f g x =
  f (g x);;

compose square inc 5;;
(* square(inc(5)) -> square(6) -> 36 *)

compose inc square 5;;
(* inc(square(5)) -> inc(25) -> 26 *)
