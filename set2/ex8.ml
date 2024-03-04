(*

Implement the following functions:
• select xs – returns a pair consisting of the smallest element of the xs list and a list of all xs elements except the smallest one.
You can also think of this procedure as returning a permutation of the list xs in which the smallest element is in the first position and the order of the remaining elements remains unchanged.

*)

let select xs =
  let rec minimum_val xs min_val =
    match xs with
    | [] -> min_val
    | head::tail ->
      if head < min_val then minimum_val tail head
      else minimum_val tail min_val
  in
  let rec remove_minimum xs min_val =
    match xs with
    | [] -> []
    | head::tail ->
      if min_val = head then tail
      else head :: remove_minimum tail min_val
  in
  let calc_min = minimum_val xs (List.hd xs)
  in
  (calc_min, remove_minimum xs calc_min);;

let rec select_sort xs =
  if xs = [] then []
  else
    let min_val, cut_xs = select xs
    in
    min_val :: select_sort cut_xs;;
