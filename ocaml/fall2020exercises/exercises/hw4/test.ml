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

(*********************************)
(*        General helpers        *)
(*********************************)

(*  Returns two lazy-evaluated expressions.
  The first modifies the internally kept boolean to true when found; should be used inside
    `ast_check_expr ~on_expression: function | <required expr> -> f1 ()
  The second returns the failed message if the boolean is still false after the AST check, concatenate lists.*)
let require_expr ?points:(points = 1) msg =
  let found = ref false in
  let found_f () =
    if !found then []
    else begin
      found := true;
      [Message (Text "Found" :: msg, Success points)]
    end
  in
  let failed_f () =
    if !found then []
    else [Message (Text "You must use" :: msg, Failure)]
  in (found_f, failed_f)

let typed_printer ty ppf v =
    Introspection.print_value ppf v ty

let tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n - 1) ((f n) :: acc)
  in
  match n with
  | 0 -> (f 0) :: []
  | 1 -> (f 1) :: (f 0) :: []
  | _ -> tab (n - 1) []



(* Takes a unit test function and returns a nice message *)
let unit_test create_account test =
    let acc = create_account () in
    let (s, pts, test_f) = test acc in
    [
        Message ([Text "Running tests:"; Text s], Informative);
        try
            let passed = test_f () in
            if passed then Message ([Text "Succeeded"], Success pts)
            else Message ([Text "Failed"], Failure)
        with
        | exn -> Message ([Text "Wrong exception"; Code (Printexc.to_string exn)], Failure)
    ]

let typed_printer ty ppf v =
    Introspection.print_value ppf v ty

(* Sample n distinct randomly-chosen items from l *)
let sample l n =
  let rec split l n = match l, n with
  | [], _ -> failwith "invalid list"
  | h :: t, 0 -> (h, t)
  | h1 :: t1, n ->
     let (h, t0) = split t1 (n - 1) in
     (h, h1 :: t0)
  in
  let rec select l n size =
    match n with
    | 0 -> []
    | _ ->
        let (h, l') = split l (Random.int size) in
        let t = select l' (n - 1) (size - 1) in
        h :: t
  in
  select l n (List.length l)

(**********************************)
(*           Question 1           *)
(**********************************)

let q1_tests pwd new_acc =
    let random_good_inst acc pwd =
    (* Helper function : calls a random instruction (expected to be) guaranteed to succeed *)
        List.nth
        [
            (fun () -> acc.deposit pwd (Random.int 10));
            (fun () -> acc.retrieve pwd 0);
            (fun () -> ignore(acc.print_balance pwd))
        ]
        (Random.int 3)
    and test_for_exc f exc =
    (* True iff f () raises exception equal to exc *)
        try f (); false
        with
        | x -> x = exc
    in
    let test_initial_balance acc =
        "Initial account configuration", 1,
        fun () -> acc.print_balance pwd = 0
    and test_deposit acc =
    (* Depositing an amount increases balance by that amount *)
        "Deposit function", 2,
        fun () ->
        begin
            let amt = acc.print_balance pwd in
            let increment = 1 + Random.int 500 in
            acc.deposit pwd increment;
            acc.print_balance pwd = amt + increment
        end
    and test_retrieve acc =
    (* Retrieving an amount <= balance decreases balance by that amount *)
        "Retrieve function", 2,
        fun () ->
        begin
            acc.deposit pwd (Random.int 500 + 1);
            let amt = acc.print_balance pwd in
            let decrement = 1 + Random.int amt in
            acc.retrieve pwd decrement;
            acc.print_balance pwd = amt - decrement
        end
    and test_wrong_pass acc =
        let test_on_new_acc f =
            let acc = new_acc "goodpass" in
            test_for_exc (f acc "badpass") wrong_pass
        in
    (* Entering the wrong password for any instruction results in an exception *)
        "Incorrect password results in exception", 1,
        fun () ->
        begin
            test_on_new_acc (fun acc pwd () -> acc.deposit pwd 0) &&
            test_on_new_acc (fun acc pwd () -> acc.retrieve pwd 0) &&
            test_on_new_acc (fun acc pwd () -> ignore @@ acc.print_balance pwd) &&
            test_on_new_acc (fun acc pwd () -> acc.update_passwd pwd pwd)
        end
    and test_overdraft acc =
    (* Retrieving more than the balance results in an exception *)
        "Retrieve more money than is in the account", 2,
        fun () ->
        begin
            acc.deposit pwd (Random.int 500 + 1);
            let amt = acc.print_balance pwd in
            test_for_exc (fun () -> acc.retrieve pwd (amt + 1)) no_money
        end
    and test_update_correct acc =
    (* Updating password and then using it does not raise exception *)
        "Updated password works on ensuing calls", 2,
        fun () ->
        begin
            acc.update_passwd pwd "goodpass";
            random_good_inst acc "goodpass";
            true
        end
    and test_update_incorrect acc =
    (* Updating password and then using previous pass raises exception *)
        "Old password does not work after updating", 1,
        fun () ->
        begin
            acc.update_passwd pwd "badpass";
            acc.update_passwd "badpass" "goodpass";
            test_for_exc (random_good_inst acc "badpass") wrong_pass
        end
    and test_over_failed acc =
    (* Calling any combination of 4 instructions with bad pass results in change_pass exc *)
        "Mistyping the password 3 times for any combination of operations locks the account", 3,
        fun () ->
        begin
            acc.update_passwd pwd "goodpass";
            test_for_exc (fun () -> acc.deposit "badpass" 0) wrong_pass &&
            test_for_exc (fun () -> acc.retrieve "badpass" 0) wrong_pass &&
            test_for_exc (fun () -> ignore @@ acc.print_balance "badpass") wrong_pass &&
            test_for_exc (fun () -> acc.deposit "goodpass" 0) too_many_attempts &&
            test_for_exc (fun () -> acc.deposit "badpass" 0) too_many_attempts &&
            test_for_exc (fun () -> acc.retrieve "goodpass" 0) too_many_attempts &&
            test_for_exc (fun () -> acc.retrieve "badpass" 0) too_many_attempts &&
            test_for_exc (fun () -> ignore @@ acc.print_balance "goodpass") too_many_attempts &&
            test_for_exc (fun () -> ignore @@ acc.print_balance "badpass") too_many_attempts &&
            (* Create a whole new account just to test update_passwd... *)
            let acc = new_acc "goodpass" in
            test_for_exc (fun () -> acc.update_passwd "badpass" "") wrong_pass &&
            test_for_exc (fun () -> acc.update_passwd "badpass" "") wrong_pass &&
            test_for_exc (fun () -> acc.update_passwd "badpass" "") wrong_pass &&
            test_for_exc (fun () -> acc.deposit "goodpass" 0) too_many_attempts
        end
    and test_over_allows_update acc =
    (* Updating password is still allowed when account is locked *)
        "Updating password is still allowed after account locks", 3,
        fun () ->
        begin
            acc.update_passwd pwd "goodpass";
            test_for_exc (random_good_inst acc "badpass") wrong_pass &&
            test_for_exc (random_good_inst acc "badpass") wrong_pass &&
            test_for_exc (random_good_inst acc "badpass") wrong_pass &&
            test_for_exc (fun () -> acc.update_passwd "badpass" "newpass") wrong_pass &&
            acc.update_passwd "goodpass" "newpass" = () &&
            test_for_exc (fun () -> acc.update_passwd "goodpass" "") wrong_pass
        end
    and test_under_failed acc =
    (* Disrupting a bad sequence with a good call resets number of attempts *)
        "Using good password resets number of allowed attempts", 3,
        fun () ->
        begin
            acc.update_passwd pwd "goodpass";
            let b =
                test_for_exc (random_good_inst acc "badpass") wrong_pass &&
                test_for_exc (random_good_inst acc "badpass") wrong_pass
            in if b = false then false else
            (random_good_inst acc "goodpass" ();
            test_for_exc (random_good_inst acc "badpass") wrong_pass &&
            test_for_exc (random_good_inst acc "badpass") wrong_pass)
        end
    and test_multiple_accounts acc =
        "Operations on multiple accounts don't affect each other", 5,
        fun () ->
        begin
            let acc' = new_acc pwd in
            let amt = Random.int 500 + 1 in
            let amt' = Random.int 500 + 1 in
            let newpwd = pwd ^ "a" in
            acc.deposit pwd amt;
            acc'.deposit pwd amt';
            acc.update_passwd pwd newpwd;
            test_for_exc (random_good_inst acc pwd) wrong_pass &&
            test_for_exc (random_good_inst acc' newpwd) wrong_pass &&
            acc.print_balance newpwd = amt &&
            acc'.print_balance pwd = amt' &&
            (* Attempt counters don't affect each other *)
            test_for_exc (random_good_inst acc' newpwd) wrong_pass &&
            test_for_exc (random_good_inst acc' newpwd) wrong_pass &&
            test_for_exc (random_good_inst acc' newpwd) wrong_pass &&
            test_for_exc (random_good_inst acc pwd) wrong_pass &&
            acc.deposit newpwd 1 = ()  && (* check for no exception *)
            test_for_exc (random_good_inst acc' pwd) too_many_attempts
        end
    in
    (* Returns a list of the tests to do *)
    [
        test_initial_balance;
        test_deposit;
        test_retrieve;
        test_wrong_pass;
        test_overdraft;
        test_update_correct;
        test_update_incorrect;
        test_over_failed;
        test_over_allows_update;
        test_under_failed;
        test_multiple_accounts
    ]

(*
original idea for a reference tester.
*)
let exercise_q1 () =
    let student_sol = lookup_student [%ty: passwd -> bank_account] "new_account" in
    match student_sol () with
    | `Unbound (_, report) -> report
    | `Found (_, report, new_acc) ->
        report @
        let pwd = "123" in
        let run_unit_test = unit_test (fun () -> new_acc pwd) in
        let unit_tests = q1_tests pwd new_acc in
        try
            List.concat @@ List.map run_unit_test unit_tests
        with exn ->
            [Message ([Text "Unexpected exception"; Code (Printexc.to_string exn); Text "when calling"; Code "new_acc"], Failure)]


let master_q1 () =
    set_progress "Grading new_account";
    Section (
        [Text "Question 1:"; Code "new_account"],
        exercise_q1 ()
    )



(**********************************)
(*          Question 2.1          *)
(**********************************)

let no_ref_failure_msg =
  Message (
      [Text "You must use";
       Code "ref";
       Text "in";
       Code "fib_I"],
      Failure
    )

let require_req_fib_i expr =
  let expr = parsetree_of_tast_expression expr in
  let found = ref false in
  let _ =
    Test_lib.ast_check_expr
      ~on_function_call: (function
        | ([%expr ref], _) -> found := true; []
        | _ -> [])
      expr
  in
  if !found then []
  else [no_ref_failure_msg]

 (* To use on reports returned by find_binding.
   If a report contains no failures, return a fully
   empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_ref_fib_i tast =
  let rec_report =
    find_binding tast "fib_I" @@
      fun _ _ expr ->
        require_req_fib_i expr
  in
  when_failure rec_report

let q2_1 tast =
    set_progress "Grading fib_I";
    let ref_report = check_ref_fib_i tast in
    let sol_report =
    test_function_1_against_solution
      [%ty : int -> fib_result] "fib_I"
      ~gen: 0
      (sample (tabulate (fun x -> x) 10) 10)
    in
    let report = 
      if snd (Report.result ref_report) then
        ref_report
      else
        sol_report
    in
  (* let report = mut_report @ sol_report in *)
    Section ([Text "Question 2.1:"; Code "fib_I"], report)


(**********************************)
(*          Question 2.2          *)
(**********************************)

let hashtbl_message name =
    [Code name; Text "called on hash table"; Code "store"]

let after_q2_appl inp stud_out sol_out =
    let updt_inp = (if inp = 1 then 2 else inp) in
    (
    if !some_store_extra_add
        then Message ([Text "Found an extra call to"; Code "Hashtbl.add"; Text "with a key that was already in the store"], Failure)
        else Message ([Text "No extra calls to"; Code "Hashtbl.add"], Success 1)
    ) ::
    if List.exists not (tabulate (Hashtbl.mem store) (updt_inp + 1))
        then [Message ([Text "Your solution did not properly use the store: the store should have entries 0 to n"], Failure)]
        else [Message ([Text "All expected entries found in the store"], Success 2)]


let require_hash_fib_memo expr =
  let found_find, failed_find = require_expr ~points: 2 (hashtbl_message "Hashtbl.find_opt")
    and found_add, failed_add = require_expr ~points: 3 (hashtbl_message "Hashtbl.add")
  in
  let expr = parsetree_of_tast_expression expr in
  let found = ref false in
  let _ =
    Test_lib.ast_check_expr
      ~on_function_call: (function
        | ([%expr Hashtbl.find_opt], (_, [%expr store]) :: _) -> found := true; found_find ()
        | ([%expr Hashtbl.add], (_, [%expr store]) :: _) -> found := true; found_add ()
        | _ -> [])
      expr
  in
  if !found then []
  else failed_find () @ failed_add ()


(* To use on reports returned by find_binding.
 If a report contains no failures, return a fully
 empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_hash_fib_memo tast =
  let rec_report =
    find_binding tast "fib_memo" @@
      fun _ _ expr ->
        require_hash_fib_memo expr
  in
  when_failure rec_report  

let master_q2_2 tast =
    set_progress "Grading fib_memo";
    let hash_report = check_hash_fib_memo tast in
    let sol_report =
      test_function_1_against_solution
        [%ty: int -> int]
        "fib_memo"
        ~before_reference: (fun _ -> Hashtbl.clear_with_ref store)
        ~before_user: (fun _ -> Hashtbl.clear_with_ref store)
        ~after: after_q2_appl
        ~gen: 5
        ~sampler: (fun () -> Random.int 35 + 2)
        (* (sample (tabulate (fun x -> x) 20) 5) *)
        []
    in
    let report = 
      if snd (Report.result hash_report) then
        hash_report
      else
        sol_report
    in
    Section (
        [Text "Question 2.2:"; Code "fib_memo"], report
    )


(**********************************)
(*          Question 2.3          *)
(**********************************)

let require_hash_memo expr =
    let found_find, failed_find = require_expr [Code "Hashtbl.find_opt"]
    and found_add, failed_add = require_expr [Code "Hashtbl.add"]
    and found_create, failed_create = require_expr [Code "Hashtbl.create"]
    in
      let expr = parsetree_of_tast_expression expr in
      let found = ref false in
      let _ =
        Test_lib.ast_check_expr
          ~on_function_call: (function
            | ([%expr Hashtbl.find_opt], _) -> found := true; found_find ()
            | ([%expr Hashtbl.find_opt], _) -> found := true; found_add ()
            | ([%expr Hashtbl.create], _) -> found := true; found_create ()
            | _ -> [])
          expr
      in
        if !found then []
        else failed_find () @ failed_add () @ failed_create ()


(* To use on reports returned by find_binding.
 If a report contains no failures, return a fully
 empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_hash_memo tast =
  let rec_report =
    find_binding tast "memo" @@
      fun _ _ expr ->
        require_hash_memo expr
  in
  when_failure rec_report 

(* Ackermann function, instrumented to count how many times it's been called *)
let ackermann count =
    let ackermann' f (m, n) = begin
        incr count;
        match m, n with
        | 0, n -> n + 1
        | m, 0 -> f (m - 1, 1)
        | m, n -> f (m - 1, f (m, n - 1))
    end
    in
    ackermann'

(* Fibonacci function, instrumented to count how many times it's been called.
   Returns a float just so that the type signature isn't int -> int. *)
let fib count =
    let fib' f n = begin
        incr count;
        match n with
        | 0 | 1 -> 1.
        | n -> f (n - 1) +. f (n - 2)
    end
    in
    fib'

(* Compare the student result against the reference solution
   and return the appropriate message *)
let check_result student_result ref_result ?(points = 1) to_string =
    if student_result = ref_result then
        Message ([Text "Correct value"; Code (to_string student_result)],
            Success points)
    else
        Message ([Text "Wrong value"; Code (to_string student_result); Break;
                  Text "(expected"; Code (to_string ref_result); Text ")"],
            Failure)

let stats_to_string = Format.asprintf "%a" (typed_printer [%ty: stats])

(* For a single input, test that the memoized function student_f:
   - Returns the correct answer
   - Updates the stats record properly
   - Calls the underlying function the correct number of times
*)
let single_test_memoized in_to_string out_to_string student_count ref_count student_f ref_f student_stats ref_stats f_name input =
    begin
        student_count := 0;
        ref_count := 0;
        Message ([Text "Testing memoized function"; Code f_name; Text "on input"; Code (in_to_string input)], Informative) ::
        try
            let student_result = student_f input in
            let ref_result = ref_f input in
            check_result student_result ref_result out_to_string ::
            Message ([Text "Checking values stored in"; Code "stats"], Informative) ::
            check_result student_stats ref_stats stats_to_string ::
            Message ([Text "Checking number of times"; Code f_name; Text "was actually called"], Informative) ::
            check_result !student_count !ref_count string_of_int ~points: 2 ::
            []
        with exn ->
            [Message ([Text "Wrong exception"; Code (Printexc.to_string exn)], Failure)]
    end

(* For a given function f that takes a reference counter and returns
   a suitable argument for memoize, test the student solution against
   the reference solution *)
let test_memoized
        (f: int ref -> ('a -> 'b) -> 'a -> 'b)
        student_sol
        ref_sol
        f_name
        (ty1: 'a Ty.ty)
        (ty2: 'b Ty.ty)
        (inputs: 'a list) =
    Message ([Text "Computing"; Code ("memo " ^ f_name)], Informative) ::
    try
        let to_string ty = Format.asprintf "%a" (typed_printer ty) in
        let student_count = ref 0 in
        let ref_count = ref 0 in
        let student_stats = { lkp = ref 0; entries = ref 0 } in
        let ref_stats = { lkp = ref 0; entries = ref 0 } in
        let student_f = student_sol (f student_count) student_stats in
        let ref_f = ref_sol (f ref_count) ref_stats in
        let unit_test =
            single_test_memoized
                (to_string ty1)
                (to_string ty2)
                student_count
                ref_count
                student_f
                ref_f
                student_stats
                ref_stats
                f_name
        in
        List.concat @@ List.map unit_test inputs
    with exn ->
        [Message ([Text "Wrong exception"; Code (Printexc.to_string exn)], Failure)]

let q2_3 (fn: int ref -> ('a -> 'b) -> 'a -> 'b) fn_name (ty: ('a -> 'b) Ty.ty) (inputs: 'a list) =
    (* Construct type ('a -> 'b) -> 'a -> 'b -> stats -> 'a -> 'b from 'a -> 'b *)
    let memo_ty = Ty.curry (Ty.curry ty ty) (Ty.curry [%ty: stats] ty) in
    let student_sol = lookup_student memo_ty "memo" in
    let ref_sol = lookup_solution memo_ty "memo" in
    match ref_sol (), student_sol () with
    | _, `Unbound (_, report) -> report
    | `Unbound (_, report), _ -> report
    | `Found (_, _, ref_sol), `Found (_, _, student_sol) ->
        let domain, range = Ty.domains ty in
        test_memoized
            fn
            student_sol
            ref_sol
            fn_name domain range
            inputs

let q2_3_ack () =
    (* Initial input: (2-3, 5-6) *)
    let m, n = (Random.int 2) + 2, (Random.int 2) + 5 in
    (* Smaller input *)
    let m', n' = m - (Random.int 2), n - (Random.int 3) in
    let n' = if (m', n') = (m, n) then n - 1 else n' in
    (* Larger input *)
    let m'' = if m = 2 then m + (Random.int 2) else m in
    let n'' = n + 1 + (Random.int 2) in
    q2_3 ackermann "ackermann" [%ty: (int * int) -> int] [(m, n); (m, n); (m', n'); (m'', n'')]

let q2_3_fib () =
    (* Initial input: 15-20 *)
    let n = (Random.int 6) + 15 in
    (* Smaller input *)
    let n' = n - 1 - (Random.int 7) in
    (* Larger input *)
    let n'' = n + 1 + (Random.int 10) in
    q2_3 fib "fibonacci" [%ty: int -> float] [n; n; n'; n'']

let master_q2_3 tast =
    set_progress "Grading memo";
    let hash_report = check_hash_memo tast in
      Section (
        [Text "Question 2.3:"; Code "memo"],
        hash_report
        @ q2_3_ack ()
        @ q2_3_fib () )



        (* Note: the store is of type (int, int) Hashtbl.t,
           and none of our test functions are of type int -> int,
           so our tests guarantee that students can't have used the
           existing global store (otherwise their code wouldn't typecheck
           in the grader).
         *)
        (* To add :
            10 points for Ackermann and 10 for fib
                -> 2 points for correct result on a "large" call
                -> 4 points for time-test of same argument
                -> 4 points for time-test of smaller argument *)


(**********************************)
(*          Question 2.4          *)
(**********************************)

let require_memo expr =
    let found_memo, failed_memo = require_expr ~points:5 [Code "memo"] in
    let expr = parsetree_of_tast_expression expr in
    let found = ref false in
    let _ =
      Test_lib.ast_check_expr
        ~on_function_call: (function
          | ([%expr memo], _) -> found := true; found_memo ()
          | _ -> [])
        expr
    in
      if !found then []
      else failed_memo ()


(* To use on reports returned by find_binding.
 If a report contains no failures, return a fully
 empty report. Otherwise return the report unchanged.
 *)
let when_failure report =
  if snd (Report.result report) then report
  else []

let check_memo tast =
  let rec_report =
    find_binding tast "fibM" @@
      fun _ _ expr ->
        require_memo expr
  in
  when_failure rec_report 

let exercise_2_4 tast =
    set_progress "Grading fibM";
    let memo_report = check_memo tast in
    let sol_report = 
      test_function_1_against_solution
        [%ty : int -> (int * stats)]
        "fibM"
        ~gen: 10
        ~sampler: (fun () -> Random.int 200)
        [0; 1; 2]
    in
    let report = memo_report @ sol_report
    in
    Section(
        [Text "Question 2.4:"; Code "fibM"],
        report ) 



(**********************************)
(*             Style              *)
(**********************************)
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



(**********************************)
(*             Master             *)
(**********************************)

let () =
    set_result @@
    ast_sanity_check code_ast @@ fun () ->
      match tast with
      | None -> [forbidden_construct_msg]
      | Some tast ->
        let q1 = master_q1 () in
        let q2_1 = q2_1 tast in
        let q2_2 = master_q2_2 tast in
        let q2_3 = master_q2_3 tast in
        let q2_4 = exercise_2_4 tast in
        set_progress "Checking style.";
        let style = style_check tast in
        [q1; q2_1; q2_2; q2_3; q2_4] @ style
    (* [
        master_q1 ();
        q2_1 ();
        master_q2_2 ();
        (* master_q2_3 (); *)
        (* exercise_2_4 () *)
    ] *)
