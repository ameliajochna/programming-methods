let rec find_min = function
  | Empty -> Failwith "blad"
  | Node (, value, _) -> value
  | Node (left, _, _) -> find_min left;;

let rec def x = function
  | Empty -> Empty
  | Node(left, value, right)
