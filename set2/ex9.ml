(*

Implement the join sort algorithm. More specifically, implement the following functions:

• split xs – returns a pair of two lists differing in length by at most 1, and containing all the elements of the xs list.
The order of the elements does not have to be maintained.

Example:
#split[8; 2; 4; 7; 4; 2; 1]
- : int list * int list = ([8; 4; 4; 1], [2; 7; 2]) ; or: ([8; 2; 4; 7]; [4; 2; 1])

• merge xs ys – for arguments that are sorted lists, returns a sorted list of all xs and ys elements.

Example:
# merge [1; 4; 4; 8] [2; 2; 7]
- : int list = [1; 2; 2; 4; 4; 7; 8]

• merge_sort xs – sorts the list using the merge sort algorithm. For lists of length greater than 1, this procedure divides the input list into two almost equal parts, sorts them recursively, and then joins the sorted results.

*)

let split xs =
  let rec divide xs l1 l2 =
    match xs with
    | [] -> (l1, l2)
    | head::tail ->
      if ((List.length xs) mod 2) = 0 then
        divide tail (head :: l1) l2
      else
        divide tail l1 (head :: l2)
  in
  divide xs [] [];;

let rec merge xs ys =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | x_head::x_tail, y_head::y_tail ->
    if x_head <= y_head then x_head::(merge x_tail ys)
    else y_head::(merge xs y_tail);;

let rec merge_sort xs =
  match xs with
  | [] -> []
  | [a] -> [a]
  | _ ->
    let l1, l2 = split xs in
    let sorted_l1 = merge_sort l1 in
    let sorted_l2 = merge_sort l2 in
    merge sorted_l1 sorted_l2;;
