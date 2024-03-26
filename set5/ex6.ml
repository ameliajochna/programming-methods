(*
We can describe formulas with the following type.

Define a to_nnf function that transforms the formula to an equivalent formula in negative normal form.
You can define helper functions, but all (mutually) recursive functions should use structured recursion.
*)

type 'v formula =
  | Var of 'v
  | Neg of 'v formula
  | Conj of 'v formula * 'v formula
  | Disj of 'v formula * 'v formula;;

let example = Conj(Neg(Var("a")), Neg(Disj(Var("b"), Neg(Var("b")))));;

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf;;

let rec to_nnf = function
  | Var(v) -> NNFLit(false, v)
  | Neg(Var(v)) -> NNFLit(true, v)
  | Neg(Neg(f)) -> to_nnf f
  | Neg(Conj(f1, f2)) -> NNFDisj(to_nnf Neg(f1), to_nnf Neg(f2))
  | Neg(Disj(f1, f2)) -> NNFConj(to_nnf Neg(f1), to_nnf Neg(f2))
  | Conj(f1, f2) -> NNFConj(to_nnf f1, to_nnf f2)
  | Disj(f1, f2) -> NNFDisj(to_nnf f1, to_nnf f2)
