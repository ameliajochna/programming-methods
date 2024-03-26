(*

Consider the binary trees from the lecture.
Draw how the t-tree defined below is represented in memory:

Implement the insert_bst function that inserts an element into the BST tree while maintaining the BST property.
Show what the memory state will look like after inserting BST value 7.
Which parts of the t-tree are shared between t-tree and insert_bst 7 t?

*)

let t =
  Node (Node (Leaf, 2, Leaf),
        5,
        (Node (Node (Leaf, 6, Leaf)),
         8,
         (Node (Leaf, 9, Leaf))));;


let rec insert_bst x = function
  | Leaf -> Node (Leaf, x, Leaf)
  | Node(l, v, r) as n ->
    if x<v then Node(insert_bst x l, v, r)
    else if x>v then Node(l, v, insert_bst x r)
    else n
