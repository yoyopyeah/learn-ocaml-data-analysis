(* TODO: Correct tests for the fact function.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here *)
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int) =
  if n < 0 then domain ()
  else (match n with
        | 0 -> 1.
        | _ -> (float_of_int n) *. (fact (n - 1)))


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10, 1), 10.);
  ((10, 2), 45.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int) =
  if n < 0 then domain ()
  else (if k > n then domain ()
        else (fact n) /. ((fact k) *. fact (n - k)))


(* TODO: Write your own tests for the ackerman function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that n, k >= 0; you should not write test cases where this assumption is violated.
*)
let ackerman_tests = [
  (* Your test cases go here *)
  ((0, 0) , 1);
  ((0, 1) , 2);
  ((1, 0) , 2);
  ((3, 4) , 125);
  ((3, 1) , 13);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k)  =
  if n < 0 || k < 0 then domain ()
  else (let rec ack n k =
          match (n, k) with
          | (0, _) -> k + 1
          | (_, 0) -> ack (n - 1) 1
          | (_, _) -> ack (n - 1) (ack n (k - 1))
        in ack n k)
   

               
(* TODO: Write a good set of tests for max_factor. *)
let is_prime_tests = [
  (2, true);  (* Lowest value *)
  (3, true);
  (47, true); (* Prime *)
  (8, false);
  (42, false);
]

(* TODO: Implement is_prime. *)
(* Checking naively that a number is prime *)
(* For grader, see learn-ocaml-repo-fall2018/hw1 *)                   
let is_prime n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  if n > 1 then is_not_divisor 2
  else domain ()


(* Revise and add additional test cases, if appropriate *)
let square_root_tests = [
    (1., 1.);
    (4., 2.);
    (14.0625, 3.75);
    (12.25, 3.5);
  ]    

(* TODO: Newton-Raphson method for computing the square root *)
(* For grader, see learn-ocaml-repo-fall2018/hw1 *)
let square_root a =
  let rec findroot x acc =
    let next = (a /. x +. x) /. 2. in
    if abs_float (x -. next) < acc then next
    else findroot next acc
  in
  if a > 0. then findroot 1. epsilon_float
  else domain ()
  
(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (4, 5);
  (5, 8)
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n = 0 then a
  else fib_aux (n - 1) b (a + b)

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  if n < 0 then domain ()
  else fib_aux n 1 1

