(*
The Map module built into the standard library provides a dictionary implemented using a balanced tree.
The comparison order used there is not the default order defined by the operators (<) and (=).
Instead, dictionaries are parameterized with an order, defined by the following signature
(Map.OrderedType):

module type OrderedType = sig type t
val compare : t -> t -> int end

For two given elements of type t, the compare function returns a negative, zero or positive value if and only if, respectively, the first element is smaller, equal to or greater than the second.
The language standard library OCaml provides compare functions for various types, including int (Int.compare), string (String.compare), and char (Char.compare).

Modify the DICT signature from the lecture so that the dictionary key type is no longer a typical dict parameter, but a fixed (though unknown) type.
The modified signature should start as follows:

module type DICT = sig
  type key
  type 'a dict
*)

module type DICT = sig
  type ('a, 'b) dict
  val empty : ('a, 'b) dict
  val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
  val remove : 'a -> ('a, 'b) dict -> ('a, 'b) dict
  val find_opt : 'a -> ('a, 'b) dict -> 'b option
  val find : 'a -> ('a, 'b) dict -> 'b
  val to_list : ('a, 'b) dict -> ('a * 'b) list
end

module type DICT = sig
  type key
  type 'a dict
  val empty : 'a dict
  val insert : key -> 'a -> 'a dict -> 'a dict
  val remove : key ->  'a dict -> 'a dict
  val find_opt : key -> 'a dict -> key option
  val find : key -> 'a dict -> 'a
  val to_list : 'a dict -> (key * 'b) list
end
