(*

Modify the insert_bst function from task 5 (which inserts an element into the BST tree) so that you can create duplicate BST trees.
You can assume that elements equal to the element at the root of the tree will go to the right subtree.

Implement the tree_sort xs function, which implements the BST tree sorting algorithm:
• Create a search tree consisting of the elements of the xs list.
• Return a list of tree elements in infix order.

*)

let tree_sort xs =
  let rec get_tree xs t =
    match xs with
    | [] -> t
    | x::xss -> get_tree xss (insert_bst x t)
    | n flatten (get_tree Leaf xs);;

let flatten t = flat_append t [];;
