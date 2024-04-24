(*
Write a function that checks whether the expression in the LET language from the lecture is closed (i.e. there are no free variables in it):
closed (e : expr) : bool = ...
*)

let closed (e : expr) : bool =
  let rec check_if_closed exp vars =
    match exp with
    | Int _ -> true
    | Bool _ -> true
    | Var x -> List.mem x vars
    | Binop(_, e1, e2) -> check_if_closed e1 vars && check_if_closed e2 vars
    | If(e1, e2, e3) -> check_if_closed e1 vars && check_if_closed e2 vars && check_if_closed e3 vars
    | Let (x, e1, e2) -> check_if_closed e1 (x::vars) && check_if_closed e2 (x::vars)
  in check_if_closed e [];;
