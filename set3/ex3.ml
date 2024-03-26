(*

Implement the build_list n f function, which constructs an n-element list by applying f to values ​​from 0 to n − 1.
More specifically:
build_list n f = [f 0; f 1; ...; f (n - 1)]

*)

let build_list n f =
  let rec building it f =
    if it = n then []
    else
      (f it)::(building (it+1) f)
  in
  building 0 f;;

let negatives n = build_list n (fun x -> -x-1);;

let reciprocals n = build_list n (fun x-> 1. /. float_of_int(x+1));;

let evens n = build_list n (fun x->2*(x+1));;

let identityM n = build_list n (fun x -> build_list n (fun y -> if y=x then 1 else 0) );;
