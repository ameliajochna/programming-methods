(*

Implement the is_sorted xs function that checks whether the given list is sorted in non-descending order.

*)

let is_sorted xs =
  if xs = [] then true
  else
    let rec check_sorted xs prev =
      if xs = [] then true
      else
        let fst = List.hd xs in
        ((prev <= fst)) && (check_sorted (List.tl xs) fst)
    in check_sorted xs (List.hd xs);;
