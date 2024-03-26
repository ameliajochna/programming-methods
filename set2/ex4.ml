(*

Implement a mem x xs function that checks whether element x is in the list xs.

*)

let rec mem x xs =
  let rec check x xs =
    match xs with
    | [] -> false
    | head::tail -> (head = x) || (check x (List.tl xs))
  in check x xs;;
