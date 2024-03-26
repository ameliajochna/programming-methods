(*

Represent sets using characteristic predicates - i.e. functions of type 'a -> bool that return true if and only if the argument belongs to the set.

Define:
• empty_set – representation of an empty set,
• singleton a – returns a set containing only element a,
• in_set a s – returns true if a belongs to the set s, otherwise the result is false,
• union s t – returns the sum of the sets s and t,
• intersect s t – returns the intersection of the sets s and t.

*)


let empty_set x = false;;

let singleton a x = x = a;;
let singleton a = (fun x-> x=a);;

let in_set a x = x a;;
let union s t = fun x -> if s x || t x;;
let intersect s t = fun x-> s x && t x;;
