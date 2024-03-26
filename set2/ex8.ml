(*

Implement the following functions:
• select xs – returns a pair consisting of the smallest element of the xs list and a list of all xs elements except the smallest one.
You can also think of this procedure as returning a permutation of the list xs in which the smallest element is in the first position and the order of the remaining elements remains unchanged.

*)

let rec select xs =
  match xs with
  | [] -> failwith "empty list"
  | [x] -> (x, [])
  | x::xss -> let t = select xss in
    if x < fst t then (x, xss)
    else (fst t, xss);;

let rec select_sort xs =
  if xs = [] then []
  else
    let min_val, cut_xs = select xs
    in
    min_val :: select_sort cut_xs;;
