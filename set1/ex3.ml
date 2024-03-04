(*

In the following expressions, locate the free and bound occurrences of the variables.
Which occurrences bind each of the related occurrences?

*)

x;;
(* free x *)

let x = 3 in x + y;;

(* x bounded in x+y, free y *)

let x = 1
and y = x + 2
in x + y;;

(* x bounded in y = x+2, y bounded in x+y *)

let x = 1 in
let y = x + 2 in
x + y;;

(* x bounded in y = x+2, y bounded in x+y *)

let f x y = x * y * z;;

(* x and y bounded in x*y*z, free z *)

let f x =
  let g y z = x * y * z in
  g (h x) z;;

(* free z, free h, x bounded in g(h x) *)
