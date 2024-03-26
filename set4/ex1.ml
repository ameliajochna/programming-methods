(*
Consider the following signature:
HUFFMAN module type = ref
type "code_tree
type "code_dict
code_tree value: 'list
dict_of_code_tree value:
value encoding: 'list ->
val decode: int list -> 'code_tree -> 'list
end
Modify the Huffman encoding implementation from the lecture so that it is defined using the Huffman functor parameterized
by modules with the signatures DICT and PRIO_QUEUE, respectively.
*)

module type PRIO_QUEUE = sig
  type ('a, 'b) pq

  val empty : ('a, 'b) pq
  val insert : 'a -> 'b -> ('a, 'b) pq -> ('a, 'b) pq
  val pop : ('a, 'b) pq -> ('a, 'b) pq
  val min_with_prio : ('a, 'b) pq -> 'a * 'b
end

module Huffman (D: DICT) (P:PRIO_QUEUE) : HUFFMAN = struct
  type 'a code_tree = ...
  type 'a code_dict = ('a, int list) D.dict
  let code_tree xs =
    freg_dict xs |> D.to_list |>
