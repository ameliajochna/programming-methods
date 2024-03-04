(*

For each of the expressions below, check whether it is typed correctly and, if so, provide its type.
Justify your answer.

*)

"foo" ^ 42;

(* Incorrect, 42 isnt a string *)

"foo" ^ string_of_int 42;;

(* string, "foo42"*)

1. = 2;;

(* Incorrect, trying to compare float and int *)

fun a -> a + 5;;

(* function from int to int *)

fun a -> if a > 5  then a else "foo";;

(* Incorrect, function cannot return different types *)

fun a b -> if a > 5 then a else b;;

(* function from two ints to int *)

fun a b ->
  let c = a = b in
  if a > 3 && b = "foo"
  then c
  else false;;

(* Incorrect, b is an int and it cant be compared with string *)

fun a -> let f a b = a * a + b * b in f(f a a) a;;

(* f a a is 2a^2 so f(f a a) a is 4a^4+a^2, correct types *)

let f a = a > 2 in
if 3 > 2 then true else f (f 2);;

(* Incorrect, f expects int but f (f 2) means f(false) so gets f bool *)
