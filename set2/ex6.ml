(*

Implement the suffixes xs function that returns all suffixes of the xs list - i.e. lists that contain,
in order and without repetitions, the elements of the xs list from the given element to the end of the list.
We consider an empty list to be a suffix of any list.

*)

let rec suffixes xs =
  match xs with
  | [] -> [[]]
  | hd::tail -> xs :: suffixes (List.tl xs);;
