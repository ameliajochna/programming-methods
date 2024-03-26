(*
Define the eval_formula function that interprets the formulas from the previous task.
Then show that eval_nnf σ (to_nnf φ) ≡ eval_formula σ φ. You can assume that the function σ always stops.
*)

type 'v formula =
  | Var of 'v
  | Neg of 'v formula
  | Conj of 'v formula * 'v formula
  | Disj of 'v formula * 'v formula;;

let rec eval_formula valuate = function
  | Var(v) -> valuate v
  | Neg(f) -> not eval_formula f
  | Conj(f1, f2) -> valuate f1 && valuate f2
  | Disj(f1, f2) -> valuate f1 || valuate f2
