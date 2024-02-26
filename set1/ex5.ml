(*
Notice that in our value calculation model we allow operators to be complex expressions. Using this observation, explain the following function:
*)

let a_plus_abs_b a b = (if b > 0 then (+) else (-)) a b;;

(* + and - are functions of two integers so if returns a function (of addition of substraction)
then following function is applied to a and b 
so its either a+b when b is positive  or a-b when b is negative or zero which results in a+abs(b)*)