(*

Following the function from the lecture that sums numbers using fold_left, define a product function that calculates the product of the list's elements.
What value should product return for an empty list?

*)

let fold_left f a xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (f acc x)
  in it xs a;;

let product xs = fold_left ( * ) 1 xs;;
