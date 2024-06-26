(*
Implement a function
from_rpn r : rpn -> expr
that compiles the expression in ONP into the abstract syntax of the arithmetic expression.
*)

type expr =
  | Int of int
  | Add of expr * expr
  | Mult of expr * expr

let rec eval (e : expr) : int =
  match e with
    | Int n -> n
    | Add (e1, e2) -> eval e1 + eval e2
    | Mult (e1, e2) -> eval e1 * eval e2

type rpn_cmd =
  | Push of int
  | RAdd
  | RMult

type rpn = rpn_cmd list

let rec to_rpn (e : expr) : rpn =
  match e with
    | Int n -> [Push n]
    | Add (e1, e2) -> to_rpn e1 @ to_rpn e2 @ [RAdd]
    | Mult (e1, e2) -> to_rpn e1 @ to_rpn e2 @ [RMult]

let rec eval_rpn (r : rpn) (s : int list) : int = match r, s with
  | [], [n] -> n
  | Push n :: r', _ -> eval_rpn r' (n :: s)
  | RAdd :: r', n1 :: n2 :: s' -> eval_rpn r' (n2 + n1 :: s')
  | RMult :: r', n1 :: n2 :: s' -> eval_rpn r' (n2 * n1 :: s')
  | _,_ -> failwith "error!"


(* eval((Add(Mult(Int 5, Int 3), Int 7))) *)
(* rpn = [Push 5; Push 3; RMult; Push 7; RAdd] *)
let from_rpn (r : rpn) : expr =
  let rec compile cur_r s =
    match cur_r, s with
    | [], [e] -> e
    | Push n :: tl, _ -> compile tl (Int n :: s)
    | RAdd :: tl, e1 :: e2 :: s' -> compile tl (Add (e2, e1) :: s')
    | RMult :: tl, e1 :: e2 :: s' -> compile tl (Mult (e2, e1) :: s')
    | _, _ -> failwith "error"
  in compile r [];;
