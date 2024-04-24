(*

Extend the LET language evaluator with the sum construction. Semantics expressions of the sum x = n to m in k
Is:
1. Calculate the value of the term n for the number n (if the term does not evaluate to a different type of value, report an error type).
2. Calculate the value of the term m for the number m (if the term does not evaluate to a different type of value, report an error type).
3. For a natural number and the range [n,m], calculate the value of the expression and substitute for the variable x. If any of these terms does not evaluate to the number, report a type error.
4. The value of the term sum x = n to m in k is the sum of all terms ̇calculated in point 3.

*)

let rec eval (e : expr) : value =
  | Sum (x, e1, e2, e3) ->
    let rec calc_sum cur_pt max_pt acc =
      if eval(Binop(Eq, cur_pt, max_pt)) = VBool(true) then Binop(Add, acc, expr_of_value(eval(subst x cur_pt e3)))
      else calc_sum (Binop(Add, cur_pt, Int(1))) max_pt (Binop(Add, acc, expr_of_value(eval(subst x cur_pt e3))))
    in eval(calc_sum e1 e2 (Int(0)))

let rec subst (x : ident) (s : expr) (e : expr) : expr =
  | Sum(y, e1, e2, e3) -> Sum (y, subst x s e1, subst x s e2, subst x s e3)
