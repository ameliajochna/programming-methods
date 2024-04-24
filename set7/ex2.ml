(*

Propose an abstract syntax in the form of a type definition in OCaml (e.g. an extension of the word en ́ from the lecture) for the following specific notations:
•for i: = n to m do ... end (*Pascal loop*)
• definite integral from k to n f(x) dx

*)

(* abstract syntax tree *)

type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq | And | Or

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Var of ident
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Let of ident * expr * expr
  | For of int * int * expr
  | Integral of int * int * expr * ident
