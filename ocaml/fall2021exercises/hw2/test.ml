(* Revert to original definitions of := and ignore, which were banned from
   use in the exercise.
*)
let (:=) = Pervasives.(:=)
let ignore = Pervasives.ignore

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

let dep_graph =
  match tast with
  | None -> None
  | Some tast ->
      try Some (dependency_graph tast)
      with exn -> None

let found_use fname =
  Message ([Text "Found a use of"; Code fname], Important)
let not_found_use fname to_impl =
  Message (
      [Text "You must use";
       Code fname;
       Text "to implement";
       Code to_impl;
       Text ".";
       Text "All of our tests will be worth only partial marks";
       Text "until you do this."],
      Failure
    )


(* Check if a function is recursive or defined using a recursive helper *)
let is_recursive rf expr =
  let open Typed_ast in
  match rf with
  | Asttypes.Recursive -> true
  | _ ->
      let found_rec = ref false in
      let expression sub expr =
        match expr.sexp_desc with
        | Sexp_let (Asttypes.Recursive, _, _) -> found_rec := true; []
        | _ -> default_checker.expression sub expr
      in
      let checker = {default_checker with expression} in
      let _ = Typed_ast_lib.ast_check_expr checker expr in
      !found_rec

let should_recurse fname =
  Message (
      [Text "The function";
       Code fname;
       Text "should be defined using recursive helpers.";
       Text "All of our tests will be worth 0 points until you correct this."],
      Failure
    )

type grade_scale = Zero | Partial | Full
type error_type = Not_Used_recursion

let check_for_fn
  ?(dont_propagate = [])
  tast impl_name fn_name num_calls num_tests =
  let error_type = ref None in
  let report =
    Typed_ast_lib.find_binding tast impl_name @@ fun rf path expr ->
      (* If the function is recursive, they should just get zero. *)
      if not (is_recursive rf expr) then
        begin
          error_type := Some Not_Used_recursion;
          [should_recurse impl_name]
        end
      else
        [found_use fn_name]
  in
  let grade_scale =
    match !error_type with
    | None -> Full
    | Some Not_Used_recursion -> Zero
  in
  (report, grade_scale)

let scaling_factor grade_scale initial =
  match grade_scale with
  | Zero -> 0
  | Full -> initial
  | Partial -> initial / 2 (* rounded down *)

let is_nan x y = (compare x nan = 0) && (compare x nan = 0)
let is_infinite x y = (compare x infinity = 0) && (compare x infinity = 0)
let is_neg_infinite x y = (compare x neg_infinity = 0) && (compare x neg_infinity = 0)
let close x y = abs_float (x -. y) < 0.0001

let compare_all_close_cases a b = (close a b) || (is_nan a b) || (is_infinite a b) || (is_neg_infinite a b)

let close_optional a b = 
  match a, b with
  | Some v1, Some v2 -> compare_all_close_cases v1 v2
  | None, None -> true
  | _, _ -> false

let same_length l1 l2 = (List.compare_lengths l1 l2) = 0
let compare_lists l1 l2 =
  same_length l1 l2 && List.for_all2 compare_all_close_cases l1 l2
let compare_lists_optional ol1 ol2 =
  match ol1, ol2 with
  | Some l1, Some l2 -> compare_lists l1 l2
  | None, None -> true
  | _, _ -> false

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

(* QUESTION 1 *)

let random_dna_seq_map key =
  match key with
  | 0 -> A 
  | 1 -> T 
  | 2 -> C 
  | _ -> G
let seq_size = Random.int 100 + 3
let dna_sampler () =
  let rec dna_sampler_inner size aux =
    match size with
    | 0 -> aux
    | n -> dna_sampler_inner (n-1) ((random_dna_seq_map (Random.int 4)) :: aux)
  in
  dna_sampler_inner seq_size []

let rec compress_nonempty l = match l with
  | [] -> [(0, A)]
  | _ -> Solution.compress l

let rec compress_singleton l = match l with
  | [] -> []
  | [x] -> [(2, x)]
  | _ -> Solution.compress l

let rec compress_shifited l = match l with
  | [] -> []
  | [x] -> [(1, x)]
  | _ -> begin
    let solution = Solution.compress l in
    let rec increment_solution elements aux =
      match elements with
      | [] -> aux
      | (n, e)::els -> increment_solution els ((n+1, e) :: aux)
    in 
      increment_solution solution []
  end

let check_for_rec_compress tast = check_for_fn tast "compress" "rec"

let q1a () =
 let mut_report =
    test_unit_tests_1
      [%ty: nucleobase list -> (int * nucleobase) list] "compress"
      [("Fails on empty lists", 1, compress_nonempty);
       ("Fails on lists with only one element", 1, compress_singleton);
       ("Fails on generic case", 1, compress_shifited)]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: nucleobase list -> (int * nucleobase) list] "compress"
      ~sampler: dna_sampler
      ~gen: 8
      [[]; [A]; [T;T;T]; [C;G;G;T;A;A]]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 1a:"; Code "compress"], report)


let compressed_dna_sampler () =
  let rec dna_sampler_inner size aux =
    match size with
    | 0 -> aux
    | n -> dna_sampler_inner (n-1) ( ((Random.int 10 + 1), (random_dna_seq_map (Random.int 4)) ) :: aux)
  in
  dna_sampler_inner seq_size []


let rec decompress_nonempty l = match l with
  | [] -> [A]
  | _ -> Solution.decompress l

let rec decompress_singleton l = match l with
  | [] -> []
  | [(1, x)] -> []
  | _ -> Solution.decompress l

let rec decompress_shifited l = match l with
  | [] -> []
  | [(1, x)] -> [x]
  | _ -> begin
    let solution = Solution.decompress l in
    let rec increment_solution elements aux =
      match elements with
      | [] -> aux
      | e::els -> increment_solution els (e :: e :: aux)
    in 
      increment_solution solution []
  end

let q1b () =
 let mut_report =
    test_unit_tests_1
      [%ty: (int * nucleobase) list -> nucleobase list] "decompress"
      [("Fails on empty lists", 1, decompress_nonempty);
       ("Fails on lists with only one element", 1, decompress_singleton);
       ("Fails on generic case", 1, decompress_shifited)]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: (int * nucleobase) list -> nucleobase list] "decompress"
      ~sampler: compressed_dna_sampler
      ~gen: 8
      [[]; [(1, A)]; [(3, T)]; [(1, C); (2, G); (1, T); (4, A)]]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 1b:"; Code "decompress"], report)

(* QUESTION 2 *)

 let sampler_exp () =
  let n = 50 in
  let rec builder h =
    match h with
    | 0 -> PLUS ( builder (Random.int n), builder (Random.int n) )
    | 1 -> MINUS ( builder (Random.int n), builder (Random.int n) )
    | 2 -> MULT ( builder (Random.int n), builder (Random.int n) )
    | 3 -> DIV ( builder (Random.int n), builder (Random.int n) )
    | 4 -> SIN ( builder (Random.int n) )
    | 5 -> COS ( builder (Random.int n) )
    | 6 -> EXP ( builder (Random.int n) )
    | _ -> FLOAT ( float_of_int (Random.int 10 + 1) )
  in 
    builder (Random.int 7)

let wrong_plus_or_minus e = 
  match e with
  | PLUS (_, _) -> (Solution.eval e) +. 1.
  | MINUS (_, _) -> (Solution.eval e) +. 1.
  | _ -> Solution.eval e

let wrong_mult_or_div e = 
  match e with
  | MULT (_, _) -> (Solution.eval e) +. 1.
  | DIV (_, _) -> (Solution.eval e) +. 1.
  | _ -> Solution.eval e

let wrong_cos_or_sin e = 
  match e with
  | COS (_) -> (Solution.eval e) +. 1.
  | SIN (_) -> (Solution.eval e) +. 1.
  | _ -> Solution.eval e

let wrong_exp e = 
  match e with
  | EXP (_) -> (Solution.eval e) +. 1.
  | _ -> Solution.eval e

let wrong_float e = 
  match e with
  | FLOAT _ -> (Solution.eval e) +. 1.
  | _ -> Solution.eval e


let q2a () =
 let mut_report =
    test_unit_tests_1
      [%ty: exp -> float] "eval"
      ~test: compare_all_close_cases
      [("Fails on PLUS or MINUS case", 1, wrong_plus_or_minus);
      ("Fails on MULT or DIV case", 1, wrong_mult_or_div);
      ("Fails on COS or SIN case", 1, wrong_cos_or_sin);
      ("Fails on EXP case", 1, wrong_exp);
      ("Fails on FLOAT case", 1, wrong_float);]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: exp -> float] "eval"
      ~test: (test_eq_ok compare_all_close_cases)
      ~sampler: sampler_exp
      ~gen: 2
      [(COS (FLOAT (Random.float 2.))); (SIN (FLOAT (Random.float 2.))); (EXP (FLOAT (Random.float 2.))); (FLOAT (Random.float 2.)); 
      (DIV (FLOAT (Random.float 2.), FLOAT 1.)); 
      (MINUS (FLOAT (Random.float 2.), FLOAT (Random.float 2.))); (PLUS (FLOAT (Random.float 2.), FLOAT (Random.float 2.)));
      (MULT (FLOAT (Random.float 2.), FLOAT (Random.float 2.))); (DIV (FLOAT (Random.float 2.), FLOAT (Random.float 2.)));]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 2a:"; Code "eval"], report)


let wrong_plus_or_minus_div_mul_instr e = 
  let shift_head_instr l =
    match l with
    | [] -> []
    | h::m::t::e -> [h] @ [t] @ [m] @ e
    | _ -> l
  in
  match e with
  | PLUS (_, _) -> shift_head_instr (Solution.to_instr e)
  | MINUS (_, _) -> shift_head_instr (Solution.to_instr e)
  | MULT (_, _) -> shift_head_instr (Solution.to_instr e)
  | DIV (_, _) -> shift_head_instr (Solution.to_instr e)
  | _ -> Solution.to_instr e

let wrong_plus_or_cos_sin_exp_instr e = 
  let shift_head_instr l =
    match l with
    | [] -> []
    | h::m::e -> [m] @ [h] @ e
    | _ -> l
  in
  match e with
  | COS (_) -> shift_head_instr (Solution.to_instr e)
  | SIN (_) -> shift_head_instr (Solution.to_instr e)
  | EXP (_) -> shift_head_instr (Solution.to_instr e)
  | _ -> Solution.to_instr e

let wrong_float_instr e = 
  let empty_instr l =
    match l with
    | [] -> []
    | [n] -> []
    | _ -> l
  in
  match e with
  | FLOAT _ -> empty_instr (Solution.to_instr e)
  | _ -> Solution.to_instr e

let q2b () =
 let mut_report =
    test_unit_tests_1
      [%ty: exp -> instruction list] "to_instr"
      [("Fails on PLUS, MINUS, MULT or DIV case", 1, wrong_plus_or_minus_div_mul_instr);
      ("Fails on SIN, COS or EXP case", 1, wrong_plus_or_cos_sin_exp_instr);
      ("Fails on FLOAT case", 2, wrong_float_instr);]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: exp -> instruction list] "to_instr"
      ~sampler: sampler_exp
      ~gen: 3
      [(COS (FLOAT (Random.float 2.))); (SIN (FLOAT (Random.float 2.))); (EXP (FLOAT (Random.float 2.))); (FLOAT (Random.float 2.)); 
      (DIV (FLOAT (Random.float 2.), FLOAT 1.)); 
      (MINUS (FLOAT (Random.float 2.), FLOAT (Random.float 2.))); (PLUS (FLOAT (Random.float 2.), FLOAT (Random.float 2.)));
      (MULT (FLOAT (Random.float 2.), FLOAT (Random.float 2.))); (DIV (FLOAT (Random.float 2.), FLOAT (Random.float 2.)));]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 2b:"; Code "to_instr"], report)


let sampler_instr () =
  let key = Random.int 7 in
  match key with
  | 0 -> Plus 
  | 1 -> Minus 
  | 2 -> Mult 
  | 3 -> Div 
  | 4 -> Sin 
  | 5 -> Cos 
  | 6 -> Exp 
  | 7 -> Float (Random.float 10.)

let sampler_stack () =
  let size = (Random.int 7 + 2) in
    let rec build_stack s aux =
    match s with
    | 0 -> aux
    | n -> build_stack (s-1) ((Random.float 10.) :: aux)
    in
      build_stack size []

let wrong_plus_minus_mult_div_single_instr ins st = 
  match ins with
  | Plus -> begin
              match st with
              | e1::e2::e -> Some []
              | _ -> Solution.instr ins st
            end
  | Minus -> begin
              match st with
              | e1::e2::e -> Some []
              | _ -> Solution.instr ins st
            end
  | Mult -> begin
              match st with
              | e1::e2::e -> Some []
              | _ -> Solution.instr ins st
            end
  | Div -> begin
    match st with
    | e1::e2::e -> Some []
    | _ -> Solution.instr ins st
  end
  | _ -> Solution.instr ins st


let wrong_cos_sin_exp_single_instr ins st = 
  match ins with
  | Cos -> begin
              match st with
              | e1::e -> Some []
              | _ -> Solution.instr ins st
            end
  | Sin -> begin
              match st with
              | e1::e -> Some []
              | _ -> Solution.instr ins st
            end
  | Exp -> begin
              match st with
              | e1::e -> Some []
              | _ -> Solution.instr ins st
            end
  | _ -> Solution.instr ins st

let wrong_float_single_instr ins st = 
  match ins with
  | Float n -> Some []
  | _ -> Solution.instr ins st


let q2c () =
 let mut_report =
    test_unit_tests_2
      [%ty: instruction -> stack -> stack option] "instr"
      ~test: compare_lists_optional
      [("Fails on Plus, Minus, Mult, Div", 1, wrong_plus_minus_mult_div_single_instr);
      ("Fails on Cos, Sin, Exp", 1, wrong_cos_sin_exp_single_instr);
      ("Fails on Float", 1, wrong_float_single_instr);]
  in
  let sol_report =
    test_function_2_against_solution
      [%ty: instruction -> stack -> stack option] "instr"
      ~sampler: (fun () -> (sampler_instr (), sampler_stack ()) )
      ~test: (test_eq_ok compare_lists_optional)
      ~gen: 7
      [ (Plus, [(Random.float 200.); (Random.float 200.)]); 
        (Minus, [(Random.float 200.); (Random.float 200.); (Random.float 200.); (Random.float 200.)]);
        (Mult, [(Random.float 200.); (Random.float 200.); (Random.float 200.); (Random.float 200.)]);
        (Div, [(Random.float 200.); ( (Random.float 200.) +. 1. ); (Random.float 200.); (Random.float 200.)]);
        (Cos, [(Random.float 2.); ( (Random.float 2.)); (Random.float 2.); (Random.float 2.)]);
        (Sin, [(Random.float 2.); ( (Random.float 2.)); (Random.float 2.); (Random.float 2.)]);
        (Exp, [(Random.float 2.); ( (Random.float 2.)); (Random.float 2.); (Random.float 2.)]);
        ((Float (Random.float 200.)), [(Random.float 2.); ( (Random.float 2.)); (Random.float 2.); (Random.float 2.)]);
      ]
  in
  (* let sol_report = scale 2 sol_report in *)
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 2c:"; Code "instr"], report)


let sampler_instr_list_builder () =
  let prebuild_chunk_2 i =
    [(Float (Random.float 2.)); (Float (Random.float 2.)); i]
  in
  let prebuild_chunk_1 i =
    [(Float (Random.float 2.)); i]
  in
  let size = Random.int 10 + 2 in
  let rec builder s aux =
    let ins = sampler_instr () in
    match s with
    | 0 -> aux
    | _ -> begin
              match ins with
              | Plus | Minus | Mult | Div -> begin
                                              match aux with
                                              | [] -> builder (s-1) (aux @ (prebuild_chunk_2 ins))
                                              | _ -> builder (s-1) (aux @ (prebuild_chunk_1 ins))
                                            end
              | Cos | Sin | Exp -> begin
                                    match aux with
                                    | [] -> builder (s-1) (aux @ (prebuild_chunk_1 ins))
                                    | _ -> builder (s-1) (aux @ [ins])
                                  end
              | Float n -> builder (s-1) (aux)
          end
  in
    builder size []

(* let sampler_instr_list_builder () =
  [Float 2.2; Float 3.3; Plus; Float 5.; Mult]
 *)

let wrong_plus_minus_mult_div_single_prog sl = 
  match sl with
  | e1 :: e2 :: ins :: e -> begin
                              match ins with
                              | Plus | Minus | Mult | Div -> begin
                                                                match (Solution.prog sl) with
                                                                | Some v -> Some (v +. 1.)
                                                                | _ -> (Solution.prog sl)
                                                              end
                              | _ -> Solution.prog sl
                            end
  | _ -> Solution.prog sl

let wrong_plus_sin_cos_exp_single_prog sl =
  let cos_sin_exp = (List.mem Cos sl) || (List.mem Sin sl) || (List.mem Exp sl) in
  if cos_sin_exp then 
    match (Solution.prog sl) with
    | Some v -> Some (v +. 1.)
    | _ -> (Solution.prog sl)
  else (Solution.prog sl)

let q2d () =
 let mut_report =
    test_unit_tests_1
      [%ty: instruction list -> float option] "prog"
      ~test: close_optional
      [("Fails on Plus, Minus, Mult, Div", 1, wrong_plus_minus_mult_div_single_prog);
      ("Fails on Sin, Cos or Exp", 1, wrong_plus_sin_cos_exp_single_prog);]     
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: instruction list -> float option] "prog"
      ~sampler: sampler_instr_list_builder 
      ~test: (test_eq_ok close_optional)
      ~gen: 5
      [[Float 1.; Float 1.; Plus]; [Float 3.; Float 1.; Minus]; [Float 3.; Float 1.; Div]; [Float 1.; Cos]]
  in
  let sol_report = scale 2 sol_report in
  let report = concat_reports mut_report sol_report in
  Section ([Text "Exercise 2d:"; Code "prog"], report)


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
    | Some tast -> [q1a (); q1b (); q2a (); q2b (); q2c (); q2d ();] @ style_check tast

