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

(* Comparing floats *)
let close x y = abs_float (x -. y) < 0.0001

(* Printing exception values *)
let typed_printer ty ppf v = Introspection.print_value ppf v ty
let print_with ty = Format.asprintf "%a" (typed_printer ty)
let string_of_exn = print_with [%ty: exn]
let string_of_path = print_with [%ty: (string list * int)]
let string_of_path_list = print_with [%ty: (string list * int) list]
let string_of_path_option = print_with [%ty: (string list * int) option]
let string_of_graph = print_with [%ty: string graph]

(* Samplers *)
let sample_metric () = float ((Random.int 20) + 1)

let city_names = [
  "Malartic";
  "Senneterre";
  "Waterville";
  "Crapaud";
  "Houston";
  "Kingston";
  "Montreal";
  "Victoria";
  "Belleoram";
  "Carbonear";
  "Revelstoke";
  "Toronto";
  "Vancouver";
  "Vernon";
  "Surrey ";
  "Lacombe";
  "Brooks";
  "Brandon";
  "Thompson";
  "Cornwall";
  "Guelph";
  "London";
  "Souris";
  "Summerside";
  "Dorval";
  "Hull";
  "Saguenay";
  "Batoche";
  "Regina";
  "Whitehorse";
  "Halifax";
  "Tuktoyaktuk";
  "Hay River";
  "Placentia";
  "Harbour Grace";
  "Ferryland";
  "Bonavista"
]

let sample_vertex = sample_cases city_names

let sample_vertices =
  sample_list ~min_size: 4 ~max_size: 7 ~dups: false sample_vertex

let sample_edge vertices () =
  let v1 = sample_cases vertices () in
  let rec loop () =
    let v2 = sample_cases vertices () in
    if v2 = v1 then loop ()
    else v2
  in
  let v2 = loop () in
  let w = ((Random.int 20) + 1) in
  (v1, v2)

(* Precondition: vertices must be a list with at least 2 elements. *)
let sample_edges vertices () =
  let len = List.length vertices in
  let min_edges, max_edges =
    match len with
    | 0
    | 1 -> invalid_arg "not enough vertices"
    | 2 -> (1, 1)
    | 3 -> (1, 3)
    | _ -> (5, (len * (len - 1)) / 2)
  in
  let edges = sample_list
    ~min_size: min_edges
    ~max_size: max_edges
    ~dups: false
    (sample_edge vertices)
    ()
  in
    let rec add_weight acc edgs =
      match edgs with
      | [] -> acc
      | (v1, v2) :: xs -> add_weight ((v1, v2, ((Random.int 20) + 1)) :: acc) xs
    in
      add_weight [] edges

let sample_nodes_and_edges () =
  let vertices = sample_vertices () in
  let edges = sample_edges vertices () in
  (vertices, edges)

let sample_graph () =
  let nodes, edges = sample_nodes_and_edges () in
  {nodes; edges}

let shuffle l =
  (* Tag each element with random bits, sort, then remove the bits *)
  let l' = List.map (fun x -> (Random.bits (), x)) l in
  List.map snd @@ List.sort compare l'

let sample_node_no_edges () =
  let (v :: vs) as nodes = sample_vertices () in
  let edges = sample_edges vs () in
  let nodes = shuffle nodes in
  ({nodes; edges}, v)

let sample_node_one_edge () =
  let (v :: vs) as nodes = sample_vertices () in
  let edges = sample_edges vs () in
  let v' = sample_cases vs () in
  let w = ((Random.int 20) + 1) in
  let edges = shuffle @@ (v, v', w) :: edges in
  let nodes = shuffle nodes in
  ({nodes; edges}, v)

(* Splits a list into two lists, with the first having size i.
   Order of the first list is *not* preserved (because we don't need it to be).
*)
let split_at l i =
  let rec aux l i acc =
    match i, l with
    | 0, _ -> (acc, l)
    | _, x :: xs -> aux xs (i - 1) (x :: acc)
  in
  aux l i []

let sample_no_path () =
  let nodes = sample_vertices () in
  let n = List.length nodes in (* n ranges from 4-6 *)
  (* Split into two components of size at least 2. *)
  let m = 2 + Random.int (n - 3) in
  let l1, l2 = split_at nodes m in
  let e1 = sample_edges l1 () in
  let e2 = sample_edges l2 () in
  let nodes = shuffle nodes in
  let edges = shuffle (e1 @ e2) in
  let v1 = sample_cases l1 () in
  let v2 = sample_cases l2 () in
  ({nodes; edges}, v1, v2)

(* Create a path from a list of distinct nodes and return the path,
   the first node in the path, and the last node in the path.
*)
let path_from_nodes nodes =
  (* Pair up elements from the two lists. l2 is expected to be one element
     shorter than l1, because it has had the head removed.
     Return the list of pairs as well as the last element in l1 (which is
     the endpoint of the path.)
  *)
  let rec aux l1 l2 =
    let w = ((Random.int 20) + 1) in
    match l1, l2 with
    | [v2], [] -> ([], v2)
    | x1 :: x1s, x2 :: x2s ->
        let (path, v2) = aux x1s x2s in
        ((x1, x2, w) :: path, v2)
    | _ -> assert false
  in
  let (path, v2) = aux nodes (List.tl nodes) in
  (path, List.hd nodes, v2)

let sample_short_path nodes () =
  sample_list
    ~min_size: 2
    ~max_size: 3
    ~dups: false
    (sample_cases nodes)
    ()

(* Precondition: nodes has at least 4 elements *)
let sample_longer_path nodes () =
  let n = List.length nodes in
  sample_list
    ~min_size: 3
    ~max_size: (n - 1)
    ~dups: false
    (sample_cases nodes)
    ()

let add_edges edges new_edges =
  let rec remove_dupl acc lst =
    match lst with
    | [] -> acc
    | (v1, v2, w) :: xs -> if (List.exists (fun (a, b, c) -> (v1 = a) && (v2 = b)) new_edges ) then (remove_dupl acc xs) else (remove_dupl ((v1, v2, w) :: acc) xs)
  (* let new_edges' = *)
    (* List.filter (fun e -> not (List.mem e edges)) new_edges *)
  in
  let new_edges' = remove_dupl [] new_edges
  in
  shuffle (new_edges' @ edges)

let sample_with_path path_sampler () =
  let nodes = sample_vertices () in
  let edges = sample_edges nodes () in
  let nodes_in_path = path_sampler nodes () in
  let (path, v1, v2) = path_from_nodes nodes in
  let edges = add_edges edges path in
  ({nodes; edges}, v1, v2)

let find_path_sampler =
  sample_alternatively
    [sample_no_path;
     sample_with_path sample_short_path;
     sample_with_path sample_longer_path]

(* Generating a large graph, for tail recursion checking. *)
let tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []
let sample_large_graph_and_path () =
  let n = 2100 + Random.int 100 in
  let nodes = tabulate string_of_int n in
  (* Create a bunch of edges (n, n + 1) so the graph is essentially a very long
     linked list
  *)
  let edges =
    let w = ((Random.int 20) + 1) in
    tabulate
      (fun n -> (string_of_int n, string_of_int (n + 1), w))
      (n - 1)
  in
  let g = {nodes; edges} in
  (g, "0", string_of_int n)

(* Postcondition messages *)
let wrong_exception exn =
  Message ([Text "Wrong exception"; Code (string_of_exn exn)], Failure)

let wrong_value to_string p =
  Message ([Text "Wrong value"; Code (to_string p)], Failure)

let correct_exception exn =
  Message (
    [Text "Correct exception"; Code (string_of_exn exn)],
    Success 1)

let correct_value to_string p =
  Message (
    [Text "Correct value"; Code (to_string p)],
    Success 1)

let found_module_msg name =
  Message (
      [Text "Found module";
       Code name;
       Text "with compatible signature."],
      Informative
    )

(* PART 2: BACKTRACKING *)

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

let compare_neighbours l1 l2 =
  let l1 = List.sort String.compare l1 in
  let l2 = List.sort String.compare l2 in
  l1 = l2

(* Mutants *)
let neighbours_fails_0 g v =
  match Solution.neighbours g v with
  | [] -> assert false
  | l -> l

let neighbours_fails_1 g v =
  match Solution.neighbours g v with
  | [_] -> assert false
  | l -> l

let neighbours_fails_many g v =
  let rec aux edges =
    match edges with
    | [] -> []
    | (v1, v2, _) :: es -> if v1 = v then [v2] else aux es
  in
  aux g.edges

let neighbours_undirected g v =
  List.fold_left
    (fun acc (v1, v2, w) ->
      if v1 = v then v2 :: acc
      else if v2 = v then v1 :: acc
      else acc)
    []
    g.edges

let rec compare_all_tuples l1 l2 =
  match l1 with
  | [] -> true
  | (s1, w1) :: xs -> (List.exists (fun (s2, w2) -> (s1 = s2) && (w1 = w2)) l2) && (compare_all_tuples xs l2)


let same_length l1 l2 = (List.compare_lengths l1 l2) = 0
let compare_lists_tuple l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | _, _ -> same_length l1 l2 && compare_all_tuples l1 l2


let test_neighbours () =
  let mut_report =
    test_unit_tests_2
      [%ty: string graph -> string -> (string * int) list] "neighbours"
      ~test: compare_lists_tuple
      [("Fails on nodes with no neighbours", 1, neighbours_fails_0);
       ("Fails on nodes with only 1 neighbour", 1, neighbours_fails_1);
       (* ("General case: stops after first neighbour found",
        1,
        neighbours_fails_many); *)
       (* ("Treats edges as undirected", 1, neighbours_undirected)] *)
       ]
  in
  let sampler () =
    let ({nodes; edges} as g) = sample_graph () in
    (g, sample_cases nodes ())
  in
  let sol_report =
    test_function_2_against_solution
      [%ty: string graph -> string -> (string * int) list]
      "neighbours"
      ~test: (test_eq_ok compare_lists_tuple)
      ~sampler
      ~gen: 6
      [sample_node_no_edges (); sample_node_one_edge ()]
  in
  let report = concat_reports mut_report sol_report in
  Section ([Text "Question 1:"; Code "neighbours"], report)



let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) =
  let rec aux_node (node, w) visited =
    if node = b then [([b] , w)]
    else if List.mem node visited then []
    (* else aux_list (Solution.neighbours g node) (node :: visited) *)
    else (let pc_list  = aux_list (Solution.neighbours g node) (node :: visited) in
            List.map (fun (path,cost) -> (node::path , cost + w)) pc_list )
  and aux_list nodes visited =
    match nodes with
    | [] -> []
    | n :: ns ->
        aux_node n visited @ aux_list ns visited
  in
  aux_list [(a, 0)] []

let check_path g a b (path: (string list * int) option) =
  let all_paths = find_all_paths g a b in
  match path with
  | None ->
      if all_paths = [] then
        correct_exception Fail
      else
        wrong_exception Fail
  | Some path ->
      if List.mem path all_paths then
        correct_value string_of_path path
      else
        wrong_value string_of_path path

(* let path_sampler () =
  let ({nodes; edges} as g) = sample_graph () in
  let (v1, v2, w) = sample_edge nodes () in
  (g, v1, v2)
 *)
let path_postcondition g a b _ result =
  let msg =
    match result with
    | Error Fail -> check_path g a b None
    | Error exn -> wrong_exception exn
    | Ok path -> check_path g a b (Some path)
  in
  [msg]

let test_find_path () =
  (* TODO: test specific cases *)
  let report =
    test_function_3_against_postcond
      path_postcondition
      [%ty: string graph -> string -> string -> (string list * int)] "find_path"
      ~sampler: find_path_sampler
      ~gen: 10
      []
  in
  let report = scale 2 report in
  Section ([Text "Question 2:"; Code "find_path"], report)


(* Tail recursion checking *)
let tailrec_info_msg =
  Message (
      [Text "Checking that"; Code "find_path'"; Text "is tail-recursive"],
      Informative
    )

let stack_overflow_failure_msg =
  Message (
    [Text "Your function";
     Code "find_path'";
     Text "overflowed the stack when called with a large input.";
     Break;
     Text "You are probably not using continuations correctly";
     Text "to implement your function."],
    Failure
  )

let unexpected_exn_failure_msg exn =
  Message (
    [Text "Your function";
     Code "find_path'";
     Text "raised the following unexpected exception";
     Text "when called with a large input:";
     Code (string_of_exn exn)],
    Failure
  )

let tailrec_failure_msg =
  Message (
    [Text "Your function";
     Code "find_path'";
     Text "was not properly tail recursive.";
     Text "All of our tests will be worth 0 until you correct this."],
    Failure
  )

let appears_tailrec_msg =
  Message (
    [Text "Your function";
     Code "find_path'";
     Text "appears to be tail-recursive."],
    Important
  )

type tl_result = Unbound | Not_tailrec | Other_error | Ok

let run_find_path' () =
  let fp_lookup =
    lookup_student
      [%ty: string graph -> string -> string -> (string list * int)] "find_path'" ()
  in
  match fp_lookup with
  | `Unbound (_, report) -> (Unbound, report)
  | `Found (_, _, find_path') ->
      (* Generate a random large input to test for a stack overflow *)
      let (g, v1, v2) = sample_large_graph_and_path () in
      let v () = find_path' g v1 v2 in
      match Test_lib.result v with
      | Error Stack_overflow -> (Not_tailrec, [stack_overflow_failure_msg])
      | Error exn -> (Other_error, [unexpected_exn_failure_msg exn])
      | Ok _ -> (Ok, [appears_tailrec_msg])

let check_tailrec () =
  let status, report = run_find_path' () in
  let report = tailrec_info_msg :: report in
  let report =
    match status with
    | Not_tailrec | Other_error -> report @ [tailrec_failure_msg]
    | _ -> report
  in
  (status, report)

(* let test_find_path' () =
  let status, tailrec_report = check_tailrec () in
  let sol_report =
    test_function_3_against_postcond
      path_postcondition
      [%ty: string graph -> string -> string -> (string list * int)] "find_path'"
      ~sampler: find_path_sampler
      ~gen: 12
      []
  in
  let factor = if status = Ok then 2 else 0 in
  let sol_report = scale factor sol_report in
  let report = tailrec_report @ sol_report in *)
  let test_find_path' () =
  (* TODO: test specific cases *)
  let report =
    test_function_3_against_postcond
      path_postcondition
      [%ty: string graph -> string -> string -> (string list * int)] "find_path'"
      ~sampler: find_path_sampler
      ~gen: 12
      []
  in
  let report = scale 2 report in
  Section ([Text "Question 3:"; Code "find_path'"], report)


(* find_all_paths *)

let check_path_list g a b (path: (string list * int) list option) =
  let all_paths = find_all_paths g a b in
  match path with
  | None ->
      if all_paths = [] then
        wrong_exception Fail
      else
        wrong_exception Fail
  | Some path_list ->
      if compare_lists_tuple path_list all_paths then
        correct_value string_of_path_list path_list
      else
        wrong_value string_of_path_list path_list

let path_list_postcondition g a b _ result =
  let msg =
    match result with
    | Error Fail -> check_path_list g a b None
    | Error exn -> wrong_exception exn
    | Ok path_list -> check_path_list g a b (Some path_list)
  in
  [msg]


let find_all_paths_ex () =
  (* TODO: test specific cases *)
  let report =
    test_function_3_against_postcond
      path_list_postcondition
      [%ty: string graph -> string -> string -> (string list * int) list] "find_all_paths"
      ~sampler: find_path_sampler
      ~gen: 13
      []
  in
  let report = scale 2 report in
  Section ([Text "Question 4:"; Code "find_all_paths"], report)


(* find_shortest_path *)

let check_path_option g a b (path: (string list * int) option option) =
  let all_paths = find_all_paths g a b in
  match path with
  | None ->
      if all_paths = [] then
        wrong_exception Fail
      else
        wrong_exception Fail
  | Some path_option -> begin match path_option with
      | None -> correct_value string_of_path_option path_option
      | Some p_opt ->
        (if List.mem p_opt all_paths then
          correct_value string_of_path_option path_option
        else
          wrong_value string_of_path_option path_option)
      end

let path_option_postcondition g a b _ result =
  let msg =
    match result with
    | Error Fail -> check_path_option g a b None
    | Error exn -> wrong_exception exn
    | Ok path_option -> check_path_option g a b (Some path_option)
  in
  [msg]


let find_shortest_path () =
  (* TODO: test specific cases *)
  let report =
    test_function_3_against_postcond
      path_option_postcondition
      [%ty: string graph -> string -> string -> (string list * int) option] "find_shortest_path"
      ~sampler: find_path_sampler
      ~gen: 10
      []
  in
  let report = scale 2 report in
  Section ([Text "Question 5:"; Code "find_shortest_path"], report)


let part2 () =
  (* Enforce evaluation order *)
  let report =
    List.map
      (fun grade -> grade ())
      [test_neighbours; test_find_path; test_find_path'; find_all_paths_ex; find_shortest_path]

  in
  Section ([Text "Exercise: Backtracking"], report)

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

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    match tast with
    | None -> [forbidden_construct_msg]
    | Some tast ->
      (* Enforce evaluation order *)
      set_progress "Grading exercise: control flow and backtracking.";
      let p2 = part2 () in
      [p2] @ style_check tast
