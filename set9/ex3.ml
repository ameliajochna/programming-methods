(*
Implement a function that generates random arithmetic expressions:

let random_expr (max_depth : int) : expr = ...
where max_depth determines the maximum depth of the generated expression tree (refer to Exercise 3 from Exercise Sheet 6).

Implement a function:


let test (max_depth : int) (n : int) : bool = ...
which generates n random expressions of maximum depth max_depth and checks whether the property from_rpn (to_rpn e) = e holds for each of them.
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

let from_rpn (r : rpn) : expr =
  let rec compile cur_r s =
    match cur_r, s with
    | [], [e] -> e
    | Push n :: tl, _ -> compile tl (Int n :: s)
    | RAdd :: tl, e1 :: e2 :: s' -> compile tl (Add (e2, e1) :: s')
    | RMult :: tl, e1 :: e2 :: s' -> compile tl (Mult (e2, e1) :: s')
    | _, _ -> failwith "error"
  in compile r [];;

let random_expr (max_depth : int) : expr =
  let rec generate cur_depth =
    if cur_depth < (max_depth - 1) then
      match (Random.int 3) with
      | 0 -> Int (Random.int 100)
      | 1 -> Add (generate (cur_depth+1), generate (cur_depth+1))
      | 2 -> Mult (generate (cur_depth+1), generate (cur_depth+1))
    else
      if cur_depth = (max_depth - 1) then Int (Random.int 100)
      else failwith "error"
    in generate 0;;

let test (max_depth : int) (n : int) : bool =
    if n = 0 then true
    else
      let e = random_expr max_depth in
      ((from_rpn (to_rpn e)) = e) && test (n - 1);;
