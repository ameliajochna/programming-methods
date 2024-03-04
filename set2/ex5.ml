(*

Implement the maximum xs function that finds the largest element in a list of float numbers.
If the xs list is empty, neg_infinity (minus infinity) is returned.

*)

let maximum xs =
  let max_element a b = if a>b then a else b in
  let rec max_rec xs n =
    if n = 0 then neg_infinity
    else max_element (List.hd xs) (max_rec (List.tl xs) (n-1)) in
  max_rec xs (List.length xs);;
