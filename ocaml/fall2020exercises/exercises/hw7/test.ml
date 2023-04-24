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
  with
  | exn -> None

let unsafe_parse s =
  let Right e = P.parse s in
  e

let is_warning item =
  match item with
  | Message (_, Warning) -> true
  | _ -> false
let style_failure_str =
  "Your code has received at least one style warning (yellow background) " ^
  "that should be fixed for full marks."
let style_failure_msg =
  Message ([Text style_failure_str], Penalty 1)

let q0 () =
  let report =
    test_unit_tests_1
      [%ty: string -> (string, exp) either] "parse"
      ~test_student_soln: false
      []
  in
  Section ([Text "Question 0:"; Code "parse"], report)

let is_successful itm =
  match itm with
  | Message (_, Success _) -> true
  | _ -> false

let hidden_test_msg report num =
  let succeeded = List.length (List.filter is_successful report) in
  if succeeded >= num then
    Message ([Text (Format.sprintf "You passed %d hidden test(s)" succeeded)], Success 1)
  else
    Message ([Text (Format.sprintf "You failed %d hidden test(s)" (num - succeeded))], Failure)

(* l1 subset l2 *)
let set_incl l1 l2 =
  List.for_all (fun x -> List.mem x l2) l1

let set_eq l1 l2 = set_incl l1 l2 && set_incl l2 l1

let q1 () =
  (* obscure var names to prevent GOP *)
  let var_1 = "dlukgfwvolefribwlkefg" in
  let var_2 = "nsfhwoifvbwvgjkbdl" in
  let var_3 = "bsiulbqwiefvsi" in
  let tests =
    [
        (Int 1, [], "", "");
        (Bool true, [], "", "");
        (Var var_1, [var_1], "", "");
        (Let ([Val (Int 3, var_1)], Int 4), [], "", "");
        (Let ([Val (Int 3, var_1)], Var var_1), [], "", "");
        (Let ([Val (Int 3, var_1)], Var var_2), [var_2], "", "");
        (Fn (var_1, None, Var var_3), [var_3], "", "");
        (If (Var var_1, Primop (Plus, [Var var_2; Var var_3]), Var var_3), [var_1; var_2; var_3], "", "");
        (Rec ("foo", TInt, Var "foo"), [], "", "");
        (Rec ("foo", TInt, Var var_3), [var_3], "", "")
    ]
  in
  let report =
    test_function_1
      [%ty: exp -> name list] "free_vars"
      tests
      ~test:(test_eq_ok set_eq)
  in
  let msg = hidden_test_msg report (List.length tests) in
  Section ([Text "Question 1:"; Code "free_vars"], [msg])

let q2 () =
  let var_1 = "dvbwehjfgbw"
  and var_2 = "wbuietlhwgt"
  and var_3 = "bvuilkwlebfrw" in
  let tests =
    [
      (Var var_1, [], "", "");
      (Let ([Val (Int 3, var_1)], Int 4), [var_1], "", "");
      (Let ([Val (Int 3, var_1)], Primop (And, [Var var_1; Bool false])), [], "", "");
      (Let ([Val (Bool true, var_1)], Let ([Val (Int 4, var_2)], Primop (Plus, [Var var_1; Int 5]))),
       [var_2], "", "");
      (Let ([Val (Int 3, var_3)], Let ([Val (Int 4, var_3)], Primop (Plus, [Var var_3; Var var_3]))),
       [var_3], "", "");
      (Let ([Val (Rec ("foo", TArrow (TInt, TInt), Fn (var_3, Some TInt, Int 3)), "foo")], Int 4),
       ["foo"; var_3], "", "");
      (Let ([Val (Rec ("foo", TArrow (TInt, TInt), Fn (var_3, Some TInt, Apply (Var "foo", Int 10))), "foo")], Int 4),
       ["foo"; var_3], "", "");
      (Let ([Val (Rec ("foo", TArrow (TInt, TInt), Fn (var_2, Some TInt, Int 3)), "foo")], Apply (Var "foo", Int 20)),
       [var_2], "", "");
      (Let ([Valtuple (Var var_3, [var_1; var_2])], Bool true),
       [var_1; var_2], "", "");
      (Let ([Valtuple (Var var_3, [var_1; var_2])], Var var_1),
       [var_2], "", "");
      (Let ([ByName (Int 100, var_2)], Var var_3), [var_2], "", "")
    ]
  in
  let report =
    test_function_1
      [%ty: exp -> name list] "unused_vars"
      tests
      ~test:(test_eq_ok set_eq)
  in
  let msg = hidden_test_msg report (List.length tests) in
  Section ([Text "Question 2:"; Code "unused_vars"], [msg])

let q3 () =
  let var_1 = "fuilwvfgweg"
  and var_2 = "bwuiglbgkejbrfl"
  and var_3 = "wbioetbgjkerb" in
  (* we don't test renaming here. it's going to be tricky. *)
  let tests =
    [
      ((Int 1, var_2), Fn (var_2, None, Bool true), Fn (var_2, None, Bool true), "", "");
      ((Int 1, var_2), Fn (var_3, None, Var var_2), Fn (var_3, None, Int 1), "", "");
      ((Int 1, var_2), Fn (var_2, None, Var var_2), Fn (var_2, None, Var var_2), "", "");
      ((Var var_1, var_1), Let ([Val (Bool true, var_1)], Var var_1), Let ([Val (Bool true, var_1)], Var var_1), "", "");
      ((Int 1, var_1), Let ([Valtuple (Var var_3, [var_1; var_2])], Var var_1), Let ([Valtuple (Var var_3, [var_1; var_2])], Var var_1), "", "");
      ((Tuple [Int 3; Int 2], var_3), Let ([Valtuple (Var var_3, [var_1; var_2])], Var var_1), Let ([Valtuple (Tuple [Int 3; Int 2], [var_1; var_2])], Var var_1), "", "");
    ]
  in
  let report =
    List.concat (List.map
                   (fun t ->
                     (* we do this to sort of make the process deterministic *)
                     reset_ctr ();
                     test_function_2 [%ty: (exp * name) -> exp -> exp] "subst" [t])
                   tests)
  in
  let msg = hidden_test_msg report (List.length tests) in
  Section ([Text "Question 3:"; Code "subst"], [msg])

let q4 () =
  let tests =
    [
      (let e = unsafe_parse "fn edukfgk : int => 2 + gblkr;" in
       (e, e, "", ""));
      (let e = unsafe_parse "let fun bgdvgf (kwhfikw : int) : int = kwhfikw + 40 in bgdvgf 30 end;" in
      (e, Int 70, "", ""))
    ]
  in
  let report =
    test_function_1 [%ty: exp -> exp] "eval" tests
  in
  let msg = hidden_test_msg report (List.length tests) in
  Section ([Text "Question 4:"; Code "eval"], [msg])

let q5 () =
  let tests =
    [
      (Ctx [], unsafe_parse "1;", TInt, "", "");
      (Ctx [], unsafe_parse "true;", TBool, "", "");
      (Ctx [], unsafe_parse "fn rbkteglr : int => rbkteglr + 200;", TArrow (TInt, TInt), "", "");
      (Ctx [], unsafe_parse "let fun qgkwejq (bkgjb : int) : int = qgkwejq bkgjb in qgkwejq 400 end;", TInt, "", "");
      (Ctx ["gkuflewf", TInt], Var "gkuflewf", TInt, "", "")
    ]
  in
  let report =
    test_function_2 [%ty: context -> exp -> typ] "infer" tests
  in
  let msg = hidden_test_msg report (List.length tests) in
  Section ([Text "Question 5:"; Code "infer"], [msg])

let load_student ty name k =
  match lookup_student ty name () with
  | `Unbound (_, report) -> report
  | `Found (_, report, f) -> [k f]

let q6 () =
  let trap f =
    try
      f ();
      true
    with
    | _ -> false
  in
  let valid =
    [
      (let x = fresh_tvar () in (x, x));
      (let x = fresh_tvar () in let y = fresh_tvar () in (x, y));
      (let x = fresh_tvar () in (x, TInt))
    ]
  in
  let invalid =
    [
      (TInt, TBool);
      (TVar (ref (Some (TArrow (TInt, TInt)))), TVar (ref (Some TInt)))
    ]
  in
  let report = load_student [%ty: typ -> typ -> unit] "unify" @@
                 fun sf ->
                 let valid_passed = List.length (List.filter (fun (t1, t2) -> trap (fun () -> sf t1 t2)) valid) in
                 let invalid_passed = List.length (List.filter (fun (t1, t2) -> not (trap (fun () -> sf t1 t2))) invalid) in
                 let total = List.length valid + List.length invalid in
                 let succeeded = valid_passed + invalid_passed in
                 if succeeded >= total then
                   Message ([Text (Format.sprintf "You passed %d hidden test(s)" succeeded)], Success 1)
                 else
                   Message ([Text (Format.sprintf "You failed %d hidden test(s)" (total - succeeded))], Failure)
  in
  Section ([Text "Question 6:"; Code "unify"], report)

let style_check tast =
  let checkers = Style_check.all_checkers ~max_match_clauses: 15 () in
  let report = Style_check.ast_style_check_structure checkers tast in
  match report with
    | [Section (title, msgs)] ->
      if List.exists is_warning msgs then
        [Section (title, style_failure_msg :: msgs)]
      else report
    | _ -> report

 
let () =
  set_result @@
    ast_sanity_check code_ast @@
      fun () ->
      match tast with
      | None -> [forbidden_construct_msg]
      | Some tast ->
         [ q0 ();
           q1 ();
           q2 ();
           (* q3 ();
            * q4 (); *)
           q5 ();
           q6 ()
         ]
         @ style_check tast
