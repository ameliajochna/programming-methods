(*
Define a neg_nnf function of type 'a nnf -> 'a nnf negating the formula in negative normal form.
Then show that neg_nnf (neg_nnf φ) ≡ φ for any formula φ.
*)

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf;;

let formula = NNFConj(NNF Disj(NNFLit(false, "a"), NNFLit(true, "b")), NNFLit(false,"c"));; (* ((not a) or (not b)) and (not c) *)

let rec neg_nnf = function
  | NNFLit(b, l) -> NNFLit(not b, l)
  | NNFConj(f1, f2) -> NNFDisj(neg_nnf f1, neg_nnf f2)
  | NNFDisj(f1, f2) -> NNFConj(neg_nnf f1, neg_nnf f2);;

let negated_formula = neg_nnf formula;;

(*
NNFDisj (NNFConj (NNFLit (true, "a"), NNFLit (false, "b")),NNFLit (true, "c"))
*)
