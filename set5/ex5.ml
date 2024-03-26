(*
Define a function eval_nnf of type ('a -> bool)-> 'a nnf -> bool that interprets the formula in negative normal form,
with a given variable evaluation (function of type 'a -> bool).
Then show that for any formula φ and evaluation σ, eval_nnf σ (neg_nnf φ) ≡ not (eval_nnf σ φ) holds.
You can assume that the function σ always stops.
*)

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf;;

let rec eval_nnf valuate formula =
  match formula with
  | NNFLit(b, l) -> if not b then valuate l else valuate l
  | NNFConj(f1, f2) -> eval_nnf valuate f1 && eval_nnf valuate f2
  | NNFDisj(f1, f2) -> eval_nnf valuate f1 || eval_nnf valuate f2
