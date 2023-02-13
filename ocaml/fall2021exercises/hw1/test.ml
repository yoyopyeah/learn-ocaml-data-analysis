open Test_lib
open Typed_ast_lib
open Report

module Mutation = Mutation_test.Make (Test_lib)
module Style_check = Style_checking.Make ()
open Mutation

(* Generating the Typed AST *)
let forbidden_construct_str =
  "Unable to process your code because you have used a language construct " ^
  "that we do not cover in this course. Please fix your code to use only " ^
  "code constructs we have covered in class."
let forbidden_construct_msg =
  Message ([Text forbidden_construct_str], Failure)
let tast =
  try Some (tast_of_parsetree_structure code_ast)
  with exn -> None

(* Sampling numbers greater than 1 *)
let random_int_plus_2 n () = Random.int n + 2

(* Comparing floats *)
let close x y = abs_float (x -. y) < 0.0001

(* Helper function to hide solution report unless mutation testing passed *)
let hidden_sol_msg =
  "The tests we have run on your code will be hidden " ^
  "until you improve your test cases."
let concat_reports mut_report ?(middle = []) sol_report =
  let sol_report =
    if passed_mutation_testing mut_report then sol_report
    else
      let (score, _) = Report.result sol_report in
      [Message ([Text hidden_sol_msg], Important);
       Message ([Text "[HIDDEN TESTS]"], Success score)]
  in
  let sol_report =
    [Section ([Text "Our tests..."], sol_report)]
  in
  mut_report @ middle @ sol_report

(* QUESTION 1a *)

let rec fact_wrong_0 n =
  if n = 0 then 0.
  else if n = 1 then 1.
  else (float n) *. fact_wrong_0 (n - 1)
let rec fact_wrong_1 n =
  if n = 0 then 1.
  else if n = 1 then 0.
  else if n = 2 then 2.
  else (float n) *. fact_wrong_1 (n - 1)
let rec fact_wrong_inductive n =
  if n = 0 then 1.
  else if n = 1 then 1.
  else (float (n - 1)) *. fact_wrong_inductive (n - 1)

let q1a () =
  let mut_report =
    test_unit_tests_1
      [%ty: int -> float] "fact"
      ~test: close
      [("Incorrect results for n = 0", 2, fact_wrong_0);
       ("Incorrect results for n = 1", 1, fact_wrong_1);
       ("Incorrect recursive case", 1, fact_wrong_inductive)]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: int -> float] "fact"
      ~test: (test_eq_ok close)
      ~sampler: (random_int_plus_2 10)
      ~gen: 4
      [0; 1]
  in
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 1a:"; Code "fact"], report)

(* QUESTION 1b *)

let binomial_wrong_0_0 a b =
  match (a, b) with
  | (0, 0) -> 0.
  | (_, _) -> Solution.binomial a b

let binomial_wrong_1_0 a b =
  match (a, b) with
  | (1, 0) -> 0.
  | (_, _) -> Solution.binomial a b

let binomial_wrong_n_k a b = 
  match (a, b) with
  | (0, 0) -> 1.
  | (1, 0) -> 1.
  | (_, _) -> (Solution.binomial a b) +. 1.


let q1b () =
  let mut_report =
    test_unit_tests_2
      [%ty: int -> int -> float] "binomial"
      ~test: close
      [("Incorrect results for n = 0 and k = 0", 2, binomial_wrong_0_0);
      ("Incorrect results for n = 1 and k = 0", 1, binomial_wrong_1_0);
      ("Incorrect results for general n, k", 1, binomial_wrong_n_k);]
  in
  let sol_report =
    test_function_2_against_solution
      [%ty: int -> int -> float] "binomial"
      ~test: (test_eq_ok close)
      ~sampler: (fun () -> (Random.int 20 + 12, Random.int 1 + 11))
      ~gen: 5
      [(0, 0); (1, 0); (2, 1)]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 1b:"; Code "binomial"], report)



(* QUESTION 1c *)

let ackerman_wrong_0_0 (a, b) = 
  match (a, b) with
  | (0, 0) -> 0
  | (_, _) -> Solution.ackerman (a, b)

let ackerman_wrong_0_1 (a, b) = 
  match (a, b) with
  | (0, 1) -> 1
  | (_, _) -> Solution.ackerman (a, b)

let ackerman_wrong_1_0 (a, b) = 
  match (a, b) with
  | (1, 0) -> 1
  | (_, _) -> Solution.ackerman (a, b)

let ackerman_wrong_n_k (a, b) = 
  match (a, b) with
  | (0, 0) -> Solution.ackerman (a, b)  
  | (0, 1) -> Solution.ackerman (a, b)  
  | (1, 0) -> Solution.ackerman (a, b)
  | (_, _) -> Solution.ackerman (a, b) + 1

let q1c () =
  let mut_report =
    test_unit_tests_1
      [%ty: int * int -> int] "ackerman"
      [("Incorrect results for (0, 0)", 2, ackerman_wrong_0_0);
      ("Incorrect results for (0, 1)", 1, ackerman_wrong_0_1);
      ("Incorrect results for (1, 0)", 1, ackerman_wrong_1_0);
      ("Incorrect results for general (n, k)", 1, ackerman_wrong_n_k);]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: int * int -> int] "ackerman"
      ~sampler: (fun () -> (Random.int 1 + 2, Random.int 1 + 10))
      ~gen: 6
      [(0, 0); (0, 1); (1, 0); (1, 1)]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 1c:"; Code "ackerman"], report)


(**Q2 is_prime**)

let is_prime_wrong_1 n = 
  if n = 3 then false else Solution.is_prime n
let is_prime_wrong_2 n = 
  if n = 2 then false else Solution.is_prime n
let is_prime_wrong_n n =
  if n = 2 then true else (not (Solution.is_prime n))


let q2 () =
  let mut_report =
    test_unit_tests_1
      [%ty: int -> bool] "is_prime"
      [("Incorrect results for n = 2", 2, is_prime_wrong_2);
      ("Incorrect results for n = 3", 1, is_prime_wrong_1);
      ("Incorrect results for general n", 1, is_prime_wrong_n);]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: int -> bool] "is_prime"
      ~sampler: (random_int_plus_2 200)
      ~gen: 5
      [2; 3]
    in
    let sol_report = scale 2 sol_report in
    let report = concat_reports mut_report sol_report in
    Section ([ Text "Exercise 2:" ; Code "is_prime" ], report)



(** Q3 Newton-Raphson **)
let square_root_1 n = 
  if n = 1. then 0. else Solution.square_root n
let square_root_4 n = 
  if n = 4. then 0. else Solution.square_root n
let square_root_wrong_n n = 
  match n with
  | 1. -> Solution.square_root n
  | 4. -> Solution.square_root n
  | _ -> Solution.square_root n +. 1.

let q3 () =
  let mut_report =
    test_unit_tests_1
      [%ty: float -> float] "square_root"
      ~test: close
      [("Incorrect results for n = 1.", 2, square_root_1);
      ("Incorrect results for n = 4.", 1, square_root_4);
      ("Incorrect results for general n", 1, square_root_wrong_n);]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: float -> float] "square_root"
      ~gen: 6
      ~test: (test_eq_ok (fun x y -> abs_float (x -. y) < 0.0001))
      [-3.2167121; 1.0]
    in
    let sol_report = scale 2 sol_report in
    let report = concat_reports mut_report sol_report in
    Section ([ Text "Exercise 3:" ; Code "square_root" ], report)



(* QUESTION 4 *)
let tailrec_info_msg =
  Message (
      [Text "Checking that"; Code "fib_aux"; Text "is tail-recursive"],
      Informative
    )
let nonrec_failure_msg =
  Message (
      [Code "fib_tl";
       Text "should not be defined using";
       Code "let rec"],
      Failure
    )
let no_fib_aux_failure_msg =
  Message (
      [Text "You must call";
       Code "fib_aux";
       Text "in";
       Code "fib_tl"],
      Failure
    )

let check_rec = function
  | Asttypes.Nonrecursive -> []
  | Asttypes.Recursive -> [nonrec_failure_msg]

let require_fib_aux expr =
  let expr = parsetree_of_tast_expression expr in
  let found = ref false in
  let _ =
    Test_lib.ast_check_expr
      ~on_function_call: (function
        | ([%expr fib_aux], _) -> found := true; []
        | _ -> [])
      expr
  in
  if !found then []
  else [no_fib_aux_failure_msg]

(* To use on reports returned by find_binding.
   If a report contains no failures, return a fully
   empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_fib_tailrec tast =
  let aux_report =
    find_binding tast "fib_aux" @@ fun _ -> check_tailcalls ~points: 0
  in
  let aux_report = tailrec_info_msg :: aux_report in
  let rec_report =
    find_binding tast "fib_tl" @@
      fun rf _ expr ->
        check_rec rf @ require_fib_aux expr
  in
  aux_report @ when_failure rec_report

let tailrec_failure_msg =
  Message (
    [Text "Your function";
     Code "fib_tl";
     Text "was not properly tail recursive.";
     Text "We will not run our tests on your code until you correct this."],
    Failure
  )

let fib_wrong_0 n =
  let rec aux n a b =
    if n = 0 then a
    else aux (n - 1) b (a + b)
  in
  let domain () = failwith "REMINDER: You should not be writing tests for undefined values." 
  in
  if n = 0 then domain ()
  else aux n 1 1
let fib_wrong_1 n =
  let rec aux n a b =
    if n = 0 then a
    else aux (n - 1) b (a + b)
  in
  if n = 1 then 0
  else aux n 1 1
let fib_1_off n =
  let rec aux n a b =
    if n = 1 then a
    else aux (n - 1) b (a + b)
  in
  if n = 0 then 1
  else aux n 1 1

let q4 tast =
  let tl_report = check_fib_tailrec tast in
  let fib_report =
    test_function_1_against_solution
      [%ty: int -> int] "fib_tl"
      ~sampler: (random_int_plus_2 50)
      ~gen: 8
      [0; 1]
  in
  let mut_report =
    test_unit_tests_1
      [%ty: int -> int] "fib_tl"
      [("Incorrect result for 0", 2, fib_wrong_0);
       ("Incorrect result for 1", 2, fib_wrong_1);
       ("Sequence is off by 1", 1, fib_1_off);
      ]
  in
  let report =
    if snd (Report.result tl_report) then
      (* Don't run our tests if not tail-recursive *)
      mut_report @ tl_report @ [tailrec_failure_msg]
    else
      concat_reports
        mut_report
        ~middle: tl_report
        (scale 2 fib_report)
  in
  Section ([Text "Exercise 4:"; Code "fib_tl"], report)


let is_warning item =
  match item with
  | Message (_, Warning) -> true
  | _ -> false
let style_failure_str =
  "Your code has received at least one style warning (yellow background) " ^
  "that should be fixed for full marks."
let style_failure_msg =
  Message ([Text style_failure_str], Penalty 1)


let style_check tast =
  let checkers = Style_check.all_checkers () in
  let report = Style_check.ast_style_check_structure checkers tast in
  match report with
    | [Section (title, msgs)] ->
      if List.exists is_warning msgs then
        [Section (title, style_failure_msg :: msgs)]
      else report
    | _ -> report

 
let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    match tast with
    | None -> [forbidden_construct_msg]
    | Some tast -> 
        [q1a (); q1b (); q1c (); q2 (); q3 (); q4 tast] @ style_check tast
