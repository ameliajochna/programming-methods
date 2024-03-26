(*
Define the is_sorted predicate: int list -> bool checking whether the list is sorted and
function insert: int -> int list -> int list inserting an element into the sorted list.
Prove that if is_sorted xs ≡ true then is_sorted (insert x xs) ≡ true.
*)

let rec is_sorted = function
  | [] -> true
  | [_] -> true
  | x::y::xs -> x<=y && is_sorted (y::xs)

let rec insert x = function
  | [] -> [x]
  | y::ys ->
    if x<=y then x::y::ys
    else y::(insert x ys)
