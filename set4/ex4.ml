(*
Create a MakeMapDict functor typed similarly to the MakeListDict functor from task 3, using the Map module dictionaries built into the OCaml standard library.
Then, using the functor you wrote, create a CharMapDict module. Remember to use with type.
*)

module MakeMapDict (M: Map.OrderedType): DICT with type key = M.t = struct
  module MapDict = Map.Make(M)
  type key = M.t
  type 'a dict = 'a MapDict.t
  let empty = MapDict.empty
