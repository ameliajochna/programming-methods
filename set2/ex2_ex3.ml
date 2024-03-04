(*

Assume that we will represent matrices of size 2 × 2 using four-element tuples.
Define the following functions and values:
• matrix_mult m n – product of two matrices.
• matrix_id – identity matrix.
• matrix_expt m k – raises matrix m to the kth power (natural). Power can be calculated by repeated multiplication.


Using these definitions, define the fib_matrix procedure that calculates the kth Fibonacci number Fk based on the relationship:
(1, 1, 1, 0)^k = (F_k+1, F_k, F_k, F_k-1)

*)

let matrix_mult m n =
  let a, b, c, d = m in
  let e, f, g, h = n in
  (a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h);;

let matrix_id = (1, 0, 0, 1);;

let rec matrix_expt m k =
  if k = 0 then matrix_id else matrix_mult m (matrix_expt m (k-1));;

let fib_matrix k =
  let snd_of_4 (_, a, _, _) = a in
  snd_of_4 (matrix_expt (1, 1, 1, 0) k);;


(*

Define matrix_expt_fast and fib_fast procedures analogous to those from the previous task, but using the fast exponentiation algorithm.

*)

let rec matrix_expt_fast m k =
  if k = 0 then matrix_id
  else
  if (k mod 2) = 0 then
    let half = (matrix_expt_fast m (k/2)) in
    matrix_mult half half
  else matrix_mult m (matrix_expt_fast m (k-1));;
