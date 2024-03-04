(*

Implement two functions that calculate the value of Fibonacci's sequence, recursive and iterative

*)

let rec fib n =
  if n = 0 then 0 else
  if n = 1 then 1 else fib (n-1) + fib (n-2);;

let fib_iter n =
  let rec it n a b =
    if n = 0 then a
    else it (n-1) b (a+b)
  in it n 0 1;;
