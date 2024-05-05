(*
We call two terms α-equivalent when they differ only in the names of the associated variables and have the same variable covering structure.

For example, the expressions
let x = 2 in let y = 5 in x + y
let y = 2 in let z = 5 in y + z

and

let x = 2 in x + y
let z = 2 in z + y

are in pairs α-equivalent, but

let x = 2 in let y = 5 in x + y
let y = 2 in let y = 5 in y + y

and

let x = 2 in x + y
let y = 2 in y + y

are not.

Implement functions
alpha_equiv : expr -> expr -> bool
which checks whether two expressions for the LET language from the lecture are α-equivalent
*)

type env3 = ident M.t

let rec alpha_equiv (e1 : expr) (e2 : expr) (env1 : env3) (env2 : env3) : bool =
  match e1, e2 with
  | Var x1, Var x2 ->
    (match (M.find_opt x1 env1), (M.find_opt x2 env2) with
      | Some s1, Some s2 -> (x1=s2)&&(x2=s1)
      | None, None -> true
      | _, _ -> false)
  | Binop(op1, e1l, e1r), Binop(op2, e2l, e2r) -> (alpha_equiv e1l e2l env1 env2) && (alpha_equiv e1r e2r env1 env2)
  | Let(x1, e1l, e1r), Let(x2, e2l, e2r) -> (alpha_equiv e1l e2l env1 env2) && (alpha_equiv e1r e2r (M.add x1 x2 env1) (M.add x2 x1 env2))
  | Int a, Int b -> (a=b)
  | Bool a, Bool b -> (a=b)
  | _, _ -> false;;
