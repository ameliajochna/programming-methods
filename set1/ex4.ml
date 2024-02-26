(*
Define a function with three integer arguments whose result is the sum of the squares of its two larger arguments.   
*)

let squares_sum = 
  fun a b c -> if c < a && c < b then a * a + b * b else (if b < a && b < c then a * a + c * c else b * b + c * c);;

