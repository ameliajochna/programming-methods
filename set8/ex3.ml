(*
For the LET language from the lecture, implement the functions.

rename_expr : expr -> expr
which transforms the expression into an α-equivalent expression in which the names of all the variables involved are different.
For example, the expression
let x = 1 in
(let y = 2 in x + y + z) + (let x = x in x)
can be transformed into an expression
let v1 = 1 in
(let v2 = 2 in v1 + v2 + z) + (let v3 = v1 in v3)
*)

type env2 = ident M.t
let rec rename_expr (e : expr) (nazwa : string) (env : env2): expr =
  match e with
  | Binop(op,e1,e2) -> Binop(op,(rename_expr e1 (nazwa^"0") env),(rename_expr e2 (nazwa^"1") env))
  | Let(x, e1, e2) -> Let(nazwa, rename_expr e1 (nazwa^"7") env, rename_expr e2 (nazwa^"2") (M.add x nazwa env))
  | Var x ->
    (match M.find_opt x env with
      | Some v -> Var v
      | None -> Var x)
  | If(e1,e2,e3) -> If(rename_expr e1 (nazwa^"4") env, rename_expr e2 (nazwa^"5") env, rename_expr e3 (nazwa^"6") env)
  | _ -> e

let rename_expr_2 (e:expr) : expr =
  rename_expr e "#" M.empty;;
end
