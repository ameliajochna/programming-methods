(*
One of the optimizations that the compiler can perform is constant propagation.
If we write an expression in the form 20 + 30 in the program, the compiler will not produce code that calculates 20 + 30,
but will only calculate the value 50 during compilation.
One way to implement this optimization is to transform the program (in abstract syntax) into a simplified but equivalent program (in abstract syntax).
For example, we would like to program

Binop (Mult, Var "x", Binop (Add, Int 20, Int 30))

transform into a program

Binop (Mult , Var "x", Int 50)
*)

module M = Map.Make(String)
type env2 = expr M.t

let vintToint a =
  match a with
  | VInt x -> x
  | _ -> failwith "blad"

let vboolTobool a =
  match a with
  | VBool x -> x
  | _ -> failwith "blad"

let rec cp (e : expr) (env : env2): expr =
  match e with
  | Int a -> Int a
  | Bool a -> Bool a
  | Binop(op, e1, e2) ->
      (match cp e1 env, cp e2 env with
      | Int a, Int b -> Int (vintToint (eval_op op (VInt a) (VInt b)))
      | Bool a, Bool b -> Bool (vboolTobool (eval_op op (VBool a) (VBool b)))
      | _, _ ->Binop(op, (cp e1 env), (cp e2 env)))
  | Var x ->
      (match M.find_opt x env with
      | Some v -> v
      | None -> Var x)
  | Let (x, e1, e2) ->
    (match cp e1 env with
    | Int a -> cp e2 (M.add x (Int a) env)
    | Bool a -> cp e2 (M.add x (Bool a) env)
    | _ -> Let(x, e1, cp e2 (M.add x (Var x) env)))
  | _ -> failwith "nie zrobione";;
