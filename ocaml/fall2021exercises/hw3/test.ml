(* Revert to original definitions of := and ignore, which were banned from
   use in the exercise.
*)
let (:=) = Pervasives.(:=)
let ignore = Pervasives.ignore

open Test_lib
open Report
open Typed_ast_lib

module Mutation = Mutation_test.Make (Test_lib)
module Style_check = Style_checking.Make ()
open Mutation

(* Generating the Typed AST and dependency graph *)
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

(* Helpers for checking required functions *)
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
let using_too_much fname =
  Message (
      [Text "It looks like you are calling";
       Code fname;
       Text "more than once in the body of your function";
       Text "when you should only need to call it once per function call.";
       Text "You will not lose marks for this, but";
       Text "you should consider how to write a simpler solution."],
      Warning
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

let should_not_recurse fname =
  Message (
      [Text "The function";
       Code fname;
       Text "should not be defined recursively or using any recursive helpers.";
       Text "All of our tests will be worth 0 points until you correct this."],
      Failure
    )

type grade_scale = Zero | Partial | Full
type error_type = Used_recursion | No_HOFs

let check_for_fn
  ?(dont_propagate = [])
  tast impl_name fn_name num_calls num_tests =
  let error_type = ref None in
  let report =
    Typed_ast_lib.find_binding tast impl_name @@ fun rf path expr ->
      (* If the function is recursive, they should just get zero. *)
      if is_recursive rf expr then
        begin
          error_type := Some Used_recursion;
          [should_not_recurse impl_name]
        end
      else
        let found =
          match dep_graph with
          (* Dependency graph generation should not fail... but if it somehow
             does, then we have to rely solely on the dynamic method.
          *)
          | None -> true
          | Some g ->
              let fn_path = path_of_id fn_name in
              depends_on ~dont_propagate g path fn_path
        in
        if found then
          (* Give them a warning if they seem to be calling the function more
             than once per function call.
          *)
          if num_calls >= (num_tests * 2) then
            [found_use fn_name; using_too_much fn_name]
          else if num_calls > 0 then
            [found_use fn_name]
          else begin
            error_type := Some No_HOFs;
            [not_found_use fn_name impl_name]
          end
        else begin
          error_type := Some No_HOFs;
          [not_found_use fn_name impl_name]
        end
  in
  let grade_scale =
    match !error_type with
    | None -> Full
    | Some Used_recursion -> Zero
    | Some No_HOFs -> Partial
  in
  (report, grade_scale)

let scaling_factor grade_scale initial =
  match grade_scale with
  | Zero -> 0
  | Full -> initial
  | Partial -> initial / 2 (* rounded down *)

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

(* Samplers *)
(* PRECONDITION: Lower must be at least 4. *)
let sample_total_drawn ?(lower = 5) ?(upper = 9) () =
  (* Total ranges from lower (inclusive) to upper (exclusive) *)
  let total = lower + Random.int ((upper - lower) + 1) in
  (* Test total = drawn only as a special case *)
  let drawn = 3 + Random.int (total - 3) in
  (total, drawn)

let sample_black_marbles drawn () =
  Random.int (drawn + 1)

let sample_dist_table () =
  let (t, d) = sample_total_drawn () in
  let b = sample_black_marbles d () in
  ((t, d), b)

(* More realistic sampling: actually decide on a number of black marbles in
   the jar, and then simulate a number of experiments.
   This prevents the issue with a lot of randomly generated result lists
   being actually impossible as independent events.
*)
let simulate_experiments total drawn =
  let black_marbles = Random.int (((total * 2) / 3) + 1) in
  (* Number of tries ranges from 2 to 4, inclusive *)
  let num_tries = 2 + Random.int 3 in
  let sampler () = Random.int total in
  (* We will just think of the jar as a list where every index less than
     black_marbles contains a black marble, and every other index contains a
     white marble.
  *)
  let simulate_experiment _ =
    let marbles =
      sample_list ~min_size: drawn ~max_size: drawn ~dups: false sampler ()
    in
    let num_black_marbles =
      List.length (List.filter (fun x -> x < black_marbles) marbles)
    in
    num_black_marbles
  in
  tabulate simulate_experiment (num_tries - 1)

let sample_dist_matrix () =
  let (t, d) = sample_total_drawn () in
  let res = simulate_experiments t d in
  ((t, d), res)

let sample_matrix () =
  let ((t, d), res) = sample_dist_matrix () in
  Solution.dist_matrix (t, d) res

(* Comparing matrices of floats *)
let close x y = abs_float (x -. y) < 0.0001
let same_length l1 l2 = (List.compare_lengths l1 l2) = 0
let compare_lists l1 l2 =
  same_length l1 l2 && List.for_all2 close l1 l2
let compare_matrices m1 m2 =
  same_length m1 m2 && List.for_all2 compare_lists m1 m2

(* Typed AST paths for the functions to implement (for dependency checking) *)
let soln_path_opt tast name =
  try Some (soln_path tast name)
  with Not_found -> None
let paths tast =
  let dist_table = soln_path_opt tast "dist_table" in
  let dist_matrix = soln_path_opt tast "dist_matrix" in
  let is_empty = soln_path_opt tast "is_empty" in
  (dist_table, dist_matrix, is_empty)

(* QUESTION 1a *)
let tabulate_nonzero f n =
  if n = 0 then []
  else tabulate f n
let tabulate_offbyone f n =
  let rec aux n acc =
    if n < 0 then acc
    else aux (n - 1) ((f n) :: acc)
  in
  if n = 0 then [f 0]
  else aux (n - 1) []

let q1a () =
  let mut_report =
    test_unit_tests_2
      [%ty: (int -> int) -> int -> int list] "tabulate"
      ~test_student_soln: false
      [("Fails for input 0", 3, tabulate_nonzero);
       ("General case: missing (f n) in the result list", 2, tabulate_offbyone)]
  in
  Section ([Text "Question 1a:"; Code "tabulate"], mut_report)

let check_for_tabulate tast = check_for_fn tast "dist_table" "tabulate"

(* QUESTION 1b *)
let q1b tast =
  let same_drawn_as_total =
    let n = 5 + Random.int 4 in
    ((n, n), sample_black_marbles n ())
  in
  (* tabulate counter *)
  let counter = ref 0 in
  let incr_by x = counter := !counter + x in
  let sol_report =
    test_function_2_against_solution
      [%ty: int * int -> int -> float list] "dist_table"
      ~test: (test_eq_ok compare_lists)
      ~before_user: (fun _ _ -> ignore (Tab.get_tab ()))
      ~after: (fun _ _ _ _ -> incr_by (Tab.get_tab ()); [])
      ~sampler: sample_dist_table
      ~gen: 4
      [same_drawn_as_total]
  in
  let (tabulate_report, grade_scale) = check_for_tabulate tast !counter 10 in
  (* Adjust scale based on tabulate usage *)
  let factor =  scaling_factor grade_scale 1 in
  let sol_report = scale factor sol_report in
  let report = tabulate_report @ sol_report in
  Section ([Text "Question 1b:"; Code "dist_table"], report)

(* QUESTION 1c *)
let is_empty_only_one_row m = m = [[]]
let is_empty_always_true _ = true
let is_empty_many_rows m =
  if m = [[]] then false
  else Solution.is_empty m

let check_for_for_all tast = check_for_fn tast "is_empty" "List.for_all"

let q1c1 tast =
  let mut_report =
    test_unit_tests_1
      [%ty: float list list -> bool] "is_empty"
      [("Fails on the input [[]]", 1, is_empty_many_rows);
       ("Returns false for matrices consisting of multiple empty rows",
        1,
        is_empty_only_one_row);
       ("Fails on non-empty matrices", 1, is_empty_always_true)]
  in
  let size_1_matrix =
    let n = Random.int 21 in
    let x = (float n) /. 20. in
    [[x]]
  in
  let empty_matrix =
    let n = 1 + Random.int 3 in
    tabulate (fun _ -> []) n
  in
  (* List.for_all counter *)
  let counter = ref 0 in
  let incr_by x = counter := !counter + x in
  let sol_report =
    test_function_1_against_solution
      [%ty: float list list -> bool] "is_empty"
      ~before_user: (fun _ -> ignore (List.get_for_all ()))
      ~after: (fun _ _ _ -> incr_by (List.get_for_all ()); [])
      ~sampler: sample_matrix
      ~gen: 4
      [[[]]; size_1_matrix; empty_matrix]
  in
  let (for_all_report, grade_scale) = check_for_for_all tast !counter 5 in
  (* Adjust scale based on List.for_all usage *)
  let factor = scaling_factor grade_scale 1 in
  let sol_report = scale factor sol_report in
  let report = concat_reports mut_report ~middle: for_all_report sol_report in
  Section ([Text "Question 1c part 1:"; Code "is_empty"], report)

let check_for_map tast exclude_path num_calls num_tests =
  let exclude_path =
    match exclude_path with
    | None -> []
    | Some p -> [p]
  in
  check_for_fn tast
    ~dont_propagate: exclude_path
    "dist_matrix" "List.map" num_calls num_tests

let q1c2 tast dist_table_path =
  (* List.map counter *)
  let counter = ref 0 in
  let incr_by x = counter := !counter + x in
  let sol_report =
    test_function_2_against_solution
      [%ty: int * int -> int list -> float list list] "dist_matrix"
      ~test: (test_eq_ok compare_matrices)
      ~before_user: (fun _ _ -> ignore (List.get_map ()))
      ~after: (fun _ _ _ _ -> incr_by (List.get_map ()); [])
      ~sampler: sample_dist_matrix
      ~gen: 5
      []
  in
  let (map_report, grade_scale) =
    check_for_map tast dist_table_path !counter 5 in
  (* Adjust scale based on List.map usage *)
  let factor = scaling_factor grade_scale 2 in
  let sol_report = scale factor sol_report in
  let report = map_report @ sol_report in
  Section ([Text "Question 1c part 2:"; Code "dist_matrix"], report)

let usage_error_msg alt1 alt2 =
  Message (
      [Text "You must use either";
       Code alt1;
       Text "or";
       Code alt2;
       Text "to implement";
       Code "combined_dist_table";
       Text ".";
       Text "All of our tests will be worth only partial marks";
       Text "until you do this."],
      Failure
    )
let usage_error used_map used_map2 used_fold_left used_fold_right =
  (* Students need to use List.map/map2 and one of the folds *)
  let r1 =
    if used_map || used_map2 then
      [Message (
          [Text "Found a use of either";
           Code "List.map";
           Text "or";
           Code "List.map2"],
          Important)]
    else
      [usage_error_msg "List.map" "List.map2"]
  in
  let r2 =
    if used_fold_left || used_fold_right then
      [Message (
          [Text "Found a use of either";
           Code "List.fold_left";
           Text "or";
           Code "List.fold_right"],
          Important)]
    else
      [usage_error_msg "List.fold_left" "List.fold_right"]
  in
  let report = r1 @ r2 in
  (report, (used_map || used_map2) && (used_fold_left || used_fold_right))

let check_for_hofs tast exclude_paths num_maps num_map2s num_fls num_frs =
  (* None of these should have failed, but just to be sure... *)
  let exclude_paths =
    List.fold_left
      (fun acc p ->
        match p with
        | None -> acc
        | Some p -> p :: acc)
      [] exclude_paths
  in
  let report =
    Typed_ast_lib.find_binding tast "combined_dist_table" @@ fun _ path _ ->
      let (report, continue) =
        match dep_graph with
        | None -> ([], true)
        | Some g ->
            let map_path = path_of_id "List.map" in
            let map2_path = path_of_id "List.map2" in
            let fl_path = path_of_id "List.fold_left" in
            let fr_path = path_of_id "List.fold_right" in
            let deps = dependencies g ~dont_propagate: exclude_paths path in
            let used p = List.mem p deps in
            usage_error
              (used map_path) (used map2_path) (used fl_path) (used fr_path)
      in
      if continue then
        let used_map = num_maps > 0 in
        let used_map2 = num_map2s > 0 in
        let used_fl = num_fls > 0 in
        let used_fr = num_frs > 0 in
        let (report, _) = usage_error used_map used_map2 used_fl used_fr in
        report
      else
        report
  in
  (report, if snd (Report.result report) then Partial else Full)

let q1d tast exclude_paths =
  (* HOF counters *)
  let counter_map = ref 0 in
  let counter_map2 = ref 0 in
  let counter_fl = ref 0 in
  let counter_fr = ref 0 in
  let incr_counters () =
    counter_map := !counter_map + (List.get_map ());
    counter_map2 := !counter_map2 + (List.get_map2 ());
    counter_fl := !counter_fl + (List.get_fold_left ());
    counter_fr := !counter_fr + (List.get_fold_right ())
  in
  let before_user _ =
    ignore (List.get_map ());
    ignore (List.get_map2 ());
    ignore (List.get_fold_left ());
    ignore (List.get_fold_right ())
  in
  let after _ _ _ = incr_counters (); [] in
  let sol_report =
    test_function_1_against_solution
      [%ty: float list list -> float list] "combined_dist_table"
      ~test: (test_eq_ok compare_lists)
      ~before_user
      ~after
      ~sampler: sample_matrix
      ~gen: 9
      [[[]; []; []]]
  in
  let (hof_report, grade_scale) =
    check_for_hofs
      tast exclude_paths
      !counter_map !counter_map2 !counter_fl !counter_fr
  in
  (* Adjust scale based on HOF usage *)
  let factor = scaling_factor grade_scale 2 in
  let sol_report = scale factor sol_report in
  let report = hof_report @ sol_report in
  Section ([Text "Question 1d:"; Code "combined_dist_table"], report)



let sampler_ingredient () = 
  let key = Random.int 6 in
    match key with
    | 0 -> Chocolate 
    | 1 -> Orange 
    | 2 -> Almonds 
    | 3 -> Vanilla 
    | 4 -> Flour
    | 5 -> BlackBeans 


let uniq_ingredients ingreds = 
  let rec aux ingr acc =
    match ingr with
    | []     -> acc
    | i::igs -> if (List.exists (fun x -> x = i) igs) then (aux igs acc) else (aux igs (acc @ [i]) )
  in
    aux ingreds []

let sampler_ingredients_list () = 
  let size = (Random.int 5) + 5 in
    let rec build_ingredient_list s acc =
      match s with
        | 0 -> acc
        | n -> build_ingredient_list (s-1) (acc @ [(sampler_ingredient ())])
    in
      uniq_ingredients (build_ingredient_list size [])

(* type cake = Slice of ingredients list | Cake of cake * cake *)
let sampler_cake () =
  let rec cake_builder h = 
    match h with
      | 0 -> Slice (sampler_ingredients_list ())
      | n -> match (Random.int 3) with 
        | 0 -> Slice (sampler_ingredients_list ())
        | _ -> Cake (cake_builder (n-1), cake_builder (n-1))
  in
    cake_builder (Random.int 5)

let ingreds_no_choc = printable_fun "[Orange; Almonds]" (fun (x: ingredients list) -> true)
let ingreds_no_flour = printable_fun "[Chocolate; Almonds]" (fun (x: ingredients list) -> true)
let ingreds_no_almonds = printable_fun "[Chocolate; Almonds]" (fun (x: ingredients list) -> false)
let sample_ingredients_fun = sample_cases [ingreds_no_choc; ingreds_no_flour; ingreds_no_almonds]

let q2a tast =
  let sol_report =
    test_function_2_against_solution
      [%ty: (ingredients list -> bool) -> cake -> bool] "all"
      ~sampler: (fun () -> sample_ingredients_fun (), sampler_cake ())
      ~gen: 5
      []
  in
  let sol_report = scale 2 sol_report in
  (* let report = concat_reports mut_report ~middle: for_all_report sol_report in *)
  Section ([Text "Question 2a:"; Code "all"], sol_report)


(* q2b *)
let no_all_failure_msg =
  Message (
      [Text "You must call";
       Code "all";
       Text "in";
       Code "is_chocolate_cake"],
      Failure
    )

let require_all expr =
  let expr = parsetree_of_tast_expression expr in
  let found = ref false in
  let _ =
    Test_lib.ast_check_expr
      ~on_function_call: (function
        | ([%expr all], _) -> found := true; []
        | _ -> [])
      expr
  in
  if !found then []
  else [no_all_failure_msg]

 (* To use on reports returned by find_binding.
   If a report contains no failures, return a fully
   empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_all_hof tast =
  let rec_report =
    find_binding tast "is_chocolate_cake" @@
      fun _ _ expr ->
        require_all expr
  in
  when_failure rec_report


let fails_on_empty_ingredients_list c =
  let rec inner cake =
    match cake with
    | Slice l -> begin
          match l with 
            | [] -> true
            | _ -> Solution.is_chocolate_cake cake
        end
    | Cake (c1, c2) -> inner c1; inner c2
  in
    inner c

let fails_on_ingredients_list_with_chocolate c = 
  let rec inner cake =
    match cake with
    | Slice l -> begin
      if (List.exists (fun el -> el = Chocolate) l) then
        false
      else 
        (Solution.is_chocolate_cake cake)
    end
    | Cake (c1, c2) -> inner c1; inner c2
  in
    inner c


let fails_on_cakes_with_slices_without_chocolate c =
  let rec inner cake = 
    match cake with
    | Slice l -> begin
      if (List.exists (fun el -> el = Chocolate) l) then
        (Solution.is_chocolate_cake cake)
      else
        true
    end
    | Cake (c1, c2) -> inner c1; inner c2
  in
    match c with 
    | Slice _ -> Solution.is_chocolate_cake c
    | _ -> inner c

let q2b tast =
  let all_report = check_all_hof tast in
  let mut_report =
    test_unit_tests_1
      [%ty: cake -> bool] "is_chocolate_cake"
      [("Fails on Slice with empty ingredients list", 1, fails_on_empty_ingredients_list);
       ("Returns false for ingredients list with Chocolate",
        1,
        fails_on_ingredients_list_with_chocolate);
       ("Returns true for Slices with ingredients list without Chocolate",
        1,
        fails_on_cakes_with_slices_without_chocolate);]
  in
  let sol_report =
    test_function_1_against_solution
      [%ty: cake -> bool] "is_chocolate_cake"
      ~sampler: sampler_cake
      ~gen: 4
      [(Cake (Slice [Orange], Slice [Almonds]));
       (Cake (Slice [Chocolate], Slice [Almonds])); (Cake (Slice [Orange], Cake (Slice [Chocolate], Slice [Chocolate]) ))]
  in
  let report =
    if snd (Report.result all_report) then
      mut_report @ all_report
    else
      concat_reports
        mut_report
        ~middle: all_report
        (sol_report)
  in
  Section ([Text "Question 2b:"; Code "is_chocolate_cake"], report)
  (* let report = concat_reports mut_report ~middle: for_all_report sol_report in *)


let t1 = printable_fun "[Orange; Almonds]" (fun (x: ingredients list) -> [Orange])
let t2 = printable_fun "[Chocolate; Almonds]" (fun (x: ingredients list) -> [Chocolate])
let t3 = printable_fun "[Chocolate; Flour]" (fun (x: ingredients list) -> [Flour])
let sample_ingredients_to_ingredients_fun = sample_cases [t1; t2; t3]

let q2c tast =
  let sol_report =
    test_function_2_against_solution
      [%ty: (ingredients list -> ingredients list) -> cake -> cake] "map"
      ~sampler: (fun () -> sample_ingredients_to_ingredients_fun (), sampler_cake ())
      ~gen: 5
      []
  in
  let sol_report = scale 2 sol_report in
  (* let report = mut_report @ sol_report in *)
  Section ([Text "Question 2c:"; Code "map"], sol_report)



(* q2d *)
let no_map_failure_msg =
  Message (
      [Text "You must call";
       Code "map";
       Text "in";
       Code "add_ingredient"],
      Failure
    )

let require_map expr =
  let expr = parsetree_of_tast_expression expr in
  let found = ref false in
  let _ =
    Test_lib.ast_check_expr
      ~on_function_call: (function
        | ([%expr map], _) -> found := true; []
        | _ -> [])
      expr
  in
  if !found then []
  else [no_map_failure_msg]

 (* To use on reports returned by find_binding.
   If a report contains no failures, return a fully
   empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_map_hof tast =
  let rec_report =
    find_binding tast "add_ingredient" @@
      fun _ _ expr ->
        require_map expr
  in
  when_failure rec_report


let fails_to_add_on_empty i c = 
  let rec inner cake =
    match cake with
    | Slice l -> begin
      match l with
      | [] -> Slice []
      | _ -> Solution.add_ingredient i cake
    end
    | Cake (c1, c2) -> inner c1; inner c2
  in 
    inner c

let fails_to_add_on_empty_cake i c = 
  let rec inner cake =
    match cake with
    | Slice l -> Slice l 
    | Cake (c1, c2) -> inner c1; inner c2
  in 
    match c with
    | Slice _ -> Solution.add_ingredient i c
    | Cake (_, _) -> inner c
    
    
let q2d tast =
  let map_report = check_map_hof tast in
  let mut_report =
    test_unit_tests_2
      [%ty: ingredients -> cake -> cake] "add_ingredient"
      [("Fails on Slice with empty ingredients list", 1, fails_to_add_on_empty);
      ("Fails on Cake with two slices", 1, fails_to_add_on_empty_cake);]
  in
  let sol_report =
    test_function_2_against_solution
      [%ty: ingredients -> cake -> cake] "add_ingredient"
      ~sampler: (fun () -> sampler_ingredient (), sampler_cake ())
      ~gen: 2
      [(Orange, (Cake (Slice[Orange; Vanilla], Slice [Chocolate])))]
  in
  let report = 
    if snd (Report.result map_report) then
      mut_report @ map_report
    else
      concat_reports
        mut_report
        ~middle: map_report
        (sol_report)
  in
  Section ([Text "Question 2d:"; Code "add_ingredient"], report)


let sampler_ingredient_list_fold_1 = 
  fun (x: ingredients list) (y: ingredients) -> Orange
let sampler_ingredient_list_fold_2 = 
  fun (x: ingredients list) (y: ingredients) -> Chocolate
let sampler_ingredient_list_fold_3 = 
  fun (x: ingredients list) (y: ingredients) -> Almonds
let sampler_ingredient_list_fold = sample_cases [sampler_ingredient_list_fold_1; 
  sampler_ingredient_list_fold_2; sampler_ingredient_list_fold_3]


let q2e tast =
  let sol_report =
    test_function_3_against_solution
      [%ty: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a] "fold_cake"
      ~sampler: (fun () -> sampler_ingredient_list_fold (), sampler_ingredient (), sampler_cake ())
      ~gen: 5
      []
  in
  let sol_report = scale 2 sol_report in
  (* let report = mut_report @ sol_report in *)
  Section ([Text "Question 2e:"; Code "fold_cake"], sol_report)

let same_length l1 l2 = (List.compare_lengths l1 l2) = 0

let rec compare_ingred_head l1 l2 acc =
  match l1 with 
  | [] -> acc
  | x::xs -> compare_ingred_head xs l2 (acc && (List.exists (fun i -> i = x) l2))

let compare_ingredients_list l1 l2 =
  same_length l1 l2 && compare_ingred_head l1 l2 true


(* q2f *)
let no_fold_cake_failure_msg =
  Message (
      [Text "You must call";
       Code "fold_cake";
       Text "in";
       Code "get_all_ingredients"],
      Failure
    )

let require_fold_cake expr =
  let expr = parsetree_of_tast_expression expr in
  let found = ref false in
  let _ =
    Test_lib.ast_check_expr
      ~on_function_call: (function
        | ([%expr fold_cake], _) -> found := true; []
        | _ -> [])
      expr
  in
  if !found then []
  else [no_fold_cake_failure_msg]

 (* To use on reports returned by find_binding.
   If a report contains no failures, return a fully
   empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_fold_cake_hof tast =
  let rec_report =
    find_binding tast "get_all_ingredients" @@
      fun _ _ expr ->
        require_fold_cake expr
  in
  when_failure rec_report



let q2f tast =
  let fold_cake_report = check_fold_cake_hof tast in
  let sol_report =
    test_function_1_against_solution
      [%ty: cake -> ingredients list] "get_all_ingredients"
      ~sampler: sampler_cake
      ~test: (test_eq_ok compare_ingredients_list)
      ~gen: 4
      [(Cake (Slice [Orange; Chocolate; Orange; Almonds], Cake (Slice [Vanilla], Cake (Slice[Flour; Vanilla; BlackBeans], Slice []))))]
  in
  let report = 
    if snd (Report.result fold_cake_report) then
      fold_cake_report
    else
      sol_report
  in
  (* let report = mut_report @ sol_report in *)
  Section ([Text "Question 2f:"; Code "get_all_ingredients"], report)



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
      let (dt_path, dm_path, ie_path) = paths tast in
      (* Enforce evaluation order *)
      set_progress "Grading question 1a.";
      let q1a = q1a () in
      set_progress "Grading question 1b.";
      let q1b = q1b tast in
      set_progress "Grading question 1c.";
      let q1c1 = q1c1 tast in
      let q1c2 = q1c2 tast dt_path in
      set_progress "Grading question 1d.";
      let q1d = q1d tast [dt_path; dm_path; ie_path] in
      set_progress "Grading question 2a.";
      let q2a = q2a tast in
      set_progress "Grading question 2b.";
      let q2b = q2b tast in
      set_progress "Grading question 2c.";
      let q2c = q2c tast in
      set_progress "Grading question 2d.";
      let q2d = q2d tast in
      set_progress "Grading question 2e.";
      let q2e = q2e tast in
      set_progress "Grading question 2f.";
      let q2f = q2f tast in
      set_progress "Checking style.";
      let style = style_check tast in
      [q1a; q1b; q1c1; q1c2; q1d; q2a; q2b; q2c; q2d; q2e; q2f] @ style
