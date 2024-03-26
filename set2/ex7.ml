(*

Implement the is_sorted xs function that checks whether the given list is sorted in non-descending order.

*)

let is_sorted xs =
  if xs = [] then true
  else
    let rec check_sorted xs prev =
      match xs with
      | [] -> true
      | head::tail -> ((prev <= head)) && (check_sorted (List.tl xs) head)
    in check_sorted xs (List.hd xs);;
