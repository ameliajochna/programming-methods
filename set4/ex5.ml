(*
Left heaps (also known as left trees) are a simple and effective data structure that implements a priority queue (which we implemented in the lecture using an inefficient sorted list structure). As in the case of a sorted list, we want to be able to find the smallest element in constant time, but we want the remaining operations (inserting, deleting the minimum and merging two queues) to work quickly - that is, in logarithmic time. For this purpose, instead of a list, we build a binary tree in which the vertices contain heap elements with weights. An additional invariant of the data structure that will enable effective implementation is that we assign a rank to each heap, which is the length of the "right spine" (i.e. the rank of the right subtree increased by 1 - or zero in the case of an empty heap), and that in each correctly formed heap, the rank of the left subtree is not less than the rank of the right subtree.
This allows us to define the following implementation:

We represent vertices using the HNode constructor, whose fields are: vertex rank, left subtree, element priority, element, right subtree. The is_valid function checks whether the heap order is preserved (using heap_ordered) and whether the rank property described above is satisfied.
Implement the function ("smart constructor") make_node. Note that make_node does not take the rank of the heap being created, but must calculate it. It also means that we have to determine in the constructor function which of the mounds should be the right and which should be the left subtree (we can, however, assume that the heap order will be preserved).
Then implement the heap_merge function to merge the two heaps. The idea behind merging heaps is as follows: if one of the heaps is empty, merging is trivial (we take the other heap). If both are non-empty, we can find the lowest priority of each element. The smaller of these two priorities and its associated element should be at the root of the resulting heap
— it's easy to find. So we have four objects:
• the element with the lowest priority (we will call it e),
• its priority (we will call it p),
• the left subtree of the heap from which the root comes e - hl
• the right subtree of the heap whose root is derived from e - hr • the second heap, h, whose root had a priority higher than e.
To create the resulting heap, now simply merge hr and h (recursively), and then create the resulting heap from the heap obtained by recursive merging, the hl heap of element e, and the priority p. When implementing heap_merge, use the make_node function.
*)

module LeftistHeap = struct
  type ('a, 'b) heap =
    | HLeaf
    | HNode of int * ('a, 'b) heap * 'a * 'b * ('a, 'b) heap

  let rank = function HLeaf -> 0 | HNode (n, _, _, _, _) -> n

  let heap_ordered p = function
    | HLeaf -> true
    | HNode (_, _, p', _, _) -> p <= p'

  let rec is_valid = function | HLeaf -> true
                              | HNode (n, l, p, v, r) ->
                                rank r <= rank l
                                && rank r + 1 = n
                                && heap_ordered p l
                                && heap_ordered p r
                                && is_valid l
                                && is_valid r

  let make_node p v l r =
    let make_node p v l r =
      if rank l >= rank r then
        HNode (rank_r + 1, l, p, v, r)
      else
        HNode (rank_l + 1, r, p, v, l)

  let rec heap_merge h1 h2 =
    match h1, h2 with
    | HLeaf, h -> h
    | h, HLeaf -> h
    | HNode (_, l1, p1, v1, r1), HNode (_, l2, p2, v2, r2) ->
      if p1 <= p2 then
        make_node p1 v1 l1 (heap_merge r1 h2)
      else
        make_node p2 v2 l2 (heap_merge h1 r2)
end
