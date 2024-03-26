(*

Implement the maximum xs function that finds the largest element in a list of float numbers.
If the xs list is empty, neg_infinity (minus infinity) is returned.

*)

let rec maximum xs =
  match xs with
  | [] -> neg_infinity
  | head::tail ->
    let rec_max = maximum (List.tl xs) in
    if rec_max > head then rec_max
    else head;;
