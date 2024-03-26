(*

Using the fold_tree function from the lecture, define the following functions:
• tree_product t – product of all values ​​in the tree,
• tree_flip t – reverse order: swapping the left and right subtrees of all nodes in the tree,
• tree_height t – tree height (number of nodes on the longest path from root to leaf),
• tree_span t – a pair consisting of the values ​​of the rightmost and leftmost nodes in the tree (i.e. the smallest and largest values ​​in the BST tree),
• preorder t – list of all elements appearing in the tree, in preorder order. This order is based on the fact that the elements of a tree with a node at its root are listed starting from the value in the node, followed by the elements of the left subtree and then the right subtree, also in preorder order. In other words, the preorder is the order in which the nodes are visited by a depth-first search (DFS) of the tree.

*)

let rec fold_tree f a t =
  match t with
  | LeafI -> a
  | NodeI (l, v, r) -> f (fold_tree f a l) v (fold_tree f a r);;

let example_tree = NodeI (NodeI (LeafI, 1, LeafI), 2, NodeI(LeafI, 3, LeafI));;

let multiply t = fold_tree (fun l v r -> v * l * r) 1 t;;

let tree_flip t =
  let flip_node acc v l r = NodeI(r, v, l) in
  fold_tree flip_node LeafI t;;
