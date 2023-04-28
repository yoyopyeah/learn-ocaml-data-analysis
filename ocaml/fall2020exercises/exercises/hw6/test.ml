(* Based on HW8 2018 *)

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

(* Comparing floats *)
let close x y = abs_float (x -. y) < 0.0001

(* Printing exception values *)
let typed_printer ty ppf v = Introspection.print_value ppf v ty
let print_with ty = Format.asprintf "%a" (typed_printer ty)
let string_of_exn = print_with [%ty: exn]


(********************************************************)
(* Random generator for arithmetic expression fragments *)
(********************************************************)

let rec build_sum h =
  if (h < 2) then
  build_prod (h - 1)
  else
  match Random.int 3 with
  | 0 -> (build_prod (h-1)) @ [PLUS] @ (build_sum (h-1))
  | 1 -> (build_prod (h-1)) @ [SUB] @ (build_sum (h-1))
  | _ -> (build_prod (h-1))

and build_prod h =
  if (h < 1) then
  build_atom (h-1)
  else
  match Random.int 3 with
  | 0 -> (build_atom (h-1)) @ [TIMES] @ (build_prod (h-1))
  | 1 -> (build_atom (h-1)) @ [DIV] @ (build_prod (h-1))
  | _ -> (build_atom (h-1))

and build_atom h =
  if (h = 0) then
  [INT ((Random.int 10) + (Random.int 2))]
  else
  match Random.int 3 with
  |0 | 1 -> [INT ((Random.int 10) + (Random.int 2))]
  |_ -> (LPAREN :: (build_sum (h-1))) @ [RPAREN]


let build_expression () =
  let h = Random.int 3 + 4 in
  (build_sum h) @ [SEMICOLON]

(* Generates random input lists that are entirely consumed by the tested function *)
let sample_parsable input_builder random_bound lower_bound () =
  let h = Random.int random_bound + lower_bound in
  input_builder h

(* Generates random input lists that aren't necessarily entirely consumed by the tested function *)
let sample_unparsable build1 build2 random_bound lower_bound () =
  let h = Random.int random_bound + lower_bound in
    match Random.int 3 with
    | 0 -> build1 h
    | 1 -> build2 h
    | _ -> build_expression ()

(* Continuations to use for testing *)
let pair_continuation = printable_fun
  "(fun toklist exp -> (toklist, exp))"
  (fun toklist exp -> (toklist, exp))

let exp_continuation = printable_fun
  "(fun toklist exp -> exp)"
  (fun toklist exp -> exp)

let constant_continuation = printable_fun
  "(fun toklist exp -> ())"
  (fun toklist exp -> ())

(* Testing functions *)

let pair_cont_msg = Message (
  [Text "Testing with continuation"; Code "(fun toklist exp -> (toklist, exp))"],
  Important
)

let exp_cont_msg = Message (
  [Text "Testing with continuation"; Code "(fun toklist exp -> exp)"],
  Important
)

let const_cont_msg = Message (
  [Text "Testing with continuation"; Code "(fun toklist exp -> ())"],
  Important
)

let test_parser_with_continuations name tests ~gen1 ~gen2 ~gen3 ~sampler =
  let add_continuation cont = List.map (fun x -> (x, cont)) tests in
  pair_cont_msg
  ::
  (test_function_2_against_solution
    [%ty: token list -> (token list -> exp -> (token list * exp)) -> (token list * exp)]
    name
    ~gen: gen1
    ~sampler: (fun () -> (sampler (), pair_continuation))
    (add_continuation pair_continuation))
  @
  exp_cont_msg
  ::
  (test_function_2_against_solution
    [%ty: token list -> (token list -> exp -> exp) -> exp]
    name
    ~gen: gen2
    ~sampler: (fun () -> (sampler (), exp_continuation))
    (add_continuation exp_continuation))
  @
  const_cont_msg
  ::
  (test_function_2_against_solution
    [%ty: token list -> (token list -> exp -> unit) -> unit]
    name
    ~gen: gen3
    ~sampler: (fun () -> (sampler (), constant_continuation))
    (add_continuation constant_continuation))

let test_sum () =
  let samplers = [
    sample_parsable build_sum 4 2;
    sample_unparsable build_prod build_atom 4 2
  ]
  in
  let report = 
    test_parser_with_continuations
      "parseSExp"
      ~gen1: 10
      ~gen2: 10
      ~gen3: 2
      ~sampler: (sample_alternatively samplers)
      [[INT 4; TIMES; INT 2; SUB; INT 3; PLUS; INT 5]]
    in
    Section
        ( [Text "Testing function 1c"; Code "parseSExp"],
          scale 1 report
      )

let test_prod () =
  let samplers = [
    sample_parsable build_prod 4 2;
    sample_unparsable build_sum build_atom 4 2
  ]
  in
  let report = 
    test_parser_with_continuations
      "parsePExp"
      ~gen1: 10
      ~gen2: 5
      ~gen3: 2
      ~sampler: (sample_alternatively samplers)
      [[INT 3; SUB; INT 2; TIMES; INT 2; PLUS; INT 4]]
    in
    Section
      ( [Text "Testing function 1b"; Code "parsePExp"] ,
        scale 1 report
      )

let test_atom () =
  let samplers = [
    sample_parsable build_atom 2 1;
    sample_unparsable build_sum build_prod 4 2
  ]
  in
  let report = 
    test_parser_with_continuations
      "parseAtom"
      ~gen1: 10
      ~gen2: 5
      ~gen3: 2
      ~sampler: (sample_alternatively samplers)
      [[LPAREN; LPAREN; INT 4; PLUS; INT 3; DIV; INT 4; RPAREN; SUB; INT 6; TIMES; INT 2; RPAREN]]
    in 
    Section (
      [Text "Testing function 1a"; Code "parseAtom"],
      scale 1 report
    )

(* let hamming_series () =
  set_progress "Attempting to determine Hamming series";
  let result =
    let reset, test = lazy_list_verbosity [%ty: int] None in
    test_function_against_solution
    [%ty: int lazy_list]
    "hamming_series"
    ~before_user: reset
    ~test: test
    ~gen: 0  (* Scaling instead *)
    [lazy_of_list [0]]
  in
    Section (
      [Text "Testing function"; Code "hamming_series"],
      scale 2 result
    )
 *)





(* ------------------------Q2--------------------------------------- *)





(* Style checking *)
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


(* Main grader *)

(* let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    match tast with
    | None -> [forbidden_construct_msg]
    | Some tast ->
      Enforce evaluation order
      set_progress "Grading exercise: control flow and backtracking.";
      let p2 = part2 () in
      [p2] @ style_check tast
 *)

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  match tast with
    | None -> [forbidden_construct_msg]
    | Some tast ->
      [
        ( test_atom () );
        ( test_prod () );
        ( test_sum () );
      ] @ style_check tast
