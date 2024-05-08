(*
Modify the test function from the previous task to have the type
test_ce : int -> int -> expr option, which returns None if all generated expressions satisfy the property
or Some e, where e is an expression that violates the property (a counterexample).
Test the functionality of this function by modifying either from_rpn or to_rpn so that the property does not hold.
*)

let test_ce (max_depth : int) (n : int) : expr option =
  if n = 0 then None
  else
    let e = random_expr max_depth in
    if (from_rpn (to_rpn e) <> e) then Some e
    else test_ce max_depth (n - 1);;
