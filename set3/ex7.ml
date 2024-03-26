(*

The simplest implementation of the flatten function from the lecture has a serious drawback - it creates large amounts of wasteland and performs redundant calculations.
This defect can be particularly well observed in the example of trees that "only grow to the left":

let left_tree_of_list xs =
List.fold_left (fun t x -> Node (t, x, Leaf)) Leaf xs
let test_tree = left_tree_of_list (build_list 20000 Fun.id)

Write another implementation of flatten that does not have this flaw. Don't use append (or the @ operator)!
Hint: first implement the two-argument flat_append function t xs, which results in a list of elements t in infix order merged with the list xs. Example:

*)

let left_tree_of_list xs =
  List.fold_left (fun t x -> Node (t, x, Leaf)) Leaf xs
let test_tree = left_tree_of_list (build_list 20000 Fun.id)


let rec flat_append t xs =
  match t with
  | Leaf -> xs
  | Node(l, v, r) -> flat_append l (v::flat_append r xs)
