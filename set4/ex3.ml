(*

Replace the definition of the ListDict module given in the lecture with the definition of the MakeListDict functor,
parameterized with a module with the Map.OrderedType signature, and returning a module with the DICT signature from task 2.
Then, using the functor you wrote, create the CharListDict module.Note that the module you just defined does not allow (among other things)
the ability to add new items to the dictionary. Why?
Correct the definition of the MakeListDict functor by modifying the signature of the returned module.
You can use the with type construction, which allows you to add a definition of a type that was abstract (hidden) to the signature. For example, DICT with type key = char means a dictionary signature in which the key type is a character type, i.e.:

sig
type key = char type 'a dict ...
end

*)

module type DICT = sig
  type key
  type 'a dict
  val empty : 'a dict
  val insert : key -> 'a -> 'a dict -> 'a dict
  val remove : key -> 'a dict -> 'a dict
  val find_opt : key -> 'a dict -> 'a option
  val find : key -> 'a dict -> 'a
  val to_list : 'a dict -> (key * 'a) list
end

module MakeListDict (Key : Map.OrderedType) : (DICT with type key = Key.t) = struct
  type key = Key.t
  type 'a dict = (key * 'a) list
  let empty = []
  let remove k d = List.filter (fun (k', _) -> Key.compare k k' <> 0) d
  let insert k v d = (k, v) :: remove k d
  let find_opt k d = List.find_opt (fun (k', _) -> Key.compare k k' = 0) d |> Option.map snd
  let find k d = List.find (fun (k', _) -> Key.compare k k' = 0) d |> snd
  let to_list d = d
end;;


module CharListDict = MakeListDict(Char);;
