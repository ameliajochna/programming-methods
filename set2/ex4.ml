(*

Implement a mem x xs function that checks whether element x is in the list xs.

*)

let rec mem x xs =
  let rec check x xs n =
    if n = 0 then false
    else ((List.hd xs) = x) || (check x (List.tl xs) (n-1))
  in check x xs (List.length xs);;
