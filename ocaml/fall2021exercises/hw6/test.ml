open Test_lib
open Report

module Mutation = Mutation_test.Make (Test_lib)
open Mutation

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
let shuffle l =
  (* Tag each element with random bits, sort, then remove the bits *)
  let l' = List.map (fun x -> (Random.bits (), x)) l in
  List.map snd @@ List.sort compare l'

let sample_simple_bool () = B (sample_bool ())

let sample_simple_int () = I (sample_int ())

let sample_name () =
  (* ASCII character codes for lowercase letters: a = 97, z = 122 *)
  let ascii = 97 + Random.int 26 in
  Char.escaped @@ Char.chr ascii

let sample_different_names () =
  let x1 = sample_name () in
  let rec loop () =
    let x2 = sample_name () in
    if x1 = x2 then loop ()
    else x2
  in
  let x2 = loop () in
  (x1, x2)

let sample_three_names () =
  sample_list
    ~min_size: 3
    ~max_size: 3
    ~dups: false
    sample_name
    ()

let sample_different_name names () =
  let rec loop () =
    let x = sample_name () in
    if List.mem x names then loop () else x
  in
  loop ()

let sample_different_name_binds bindings () =
  let names = List.map (fun (x, _) -> x) bindings in
  sample_different_name names ()

let sample_atomic_type = sample_cases [Int; Bool]

let (sample_simple_type, sample_type) =
  let rec sample_types h =
    sample_list
      ~min_size: 1
      ~max_size: 3
      (fun () -> builder h)
  and builder h =
    match h with
    | 0 -> sample_atomic_type ()
    | _ ->
        match Random.int 2 with
        | 0 -> sample_atomic_type ()
        | _ -> Arrow (sample_types (h - 1) (), builder (h - 1))
  in
  let sample_simple_type () = builder (Random.int 2) in
  let sample_type () = builder (1 + Random.int 2) in
  (sample_simple_type, sample_type)

let sample_atomic_type = sample_cases [Int; Bool]

let sample_atomic_type_and_value () =
  if Random.bool () then (Int, sample_simple_int ())
  else (Bool, sample_simple_bool ())

(* Sampling bindings for Fn expressions *)
let sample_bindings ?(min = 1) ?(max = 3) () =
  let names =
    sample_list
      ~min_size: min
      ~max_size: max
      ~dups: false
      sample_name
      ()
  in
  List.map (fun x -> (x, sample_simple_type ())) names

let sample_simple_expr () =
  match Random.bool () with
  | true -> sample_simple_bool ()
  | false -> sample_simple_int ()

let sample_simple_exprs () =
  sample_list
    ~min_size: 1
    ~max_size: 3
    sample_simple_expr
    ()

let sample_let_with_unused () =
  let name = sample_name () in
  Let (name, sample_simple_expr (), sample_simple_expr ())

let sample_primop =
  sample_cases [Equals; LessThan; Plus; Minus; Times; Negate]

(* Sample an expression invoking a primop with one of the sub-expressions
   being exp.
*)
let sample_primop_expr exp =
  let primop = sample_primop () in
  match primop with
  | Negate -> Primop (Negate, [exp])
  | _ ->
      let other = I (Random.int 10) in
      let args =
        if Random.bool () then [exp; other]
        else [other; exp]
      in
      Primop (primop, args)

(* Sample an expression invoking a binary primop on the given expressions. *)
let sample_primop_exprs exp1 exp2 =
  let primop = sample_cases [Plus; Minus; Times] () in
  let args =
    if Random.bool () then [exp1; exp2]
    else [exp2; exp1]
  in
  Primop (primop, args)

let sample_int_primop () =
  sample_primop_exprs (sample_simple_int ()) (sample_simple_int ())

let make_list f n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (f () :: acc)
  in
  aux n []

let compare_ignoring_order l1 l2 =
  let l1' = List.sort compare l1 in
  let l2' = List.sort compare l2 in
  l1' = l2'

(********** DE BRUIJN **********)

(** Maps a function over an option. *)
let omap f = function
  | Some x -> Some (f x)
  | None -> None

(** Finds the index of the specified element in a list. *)
let rec index_of y = function
  | [] -> None
  | x :: xs when x = y -> Some 0
  | x :: xs -> omap (fun i -> i + 1) @@ index_of y xs

(** Converts an expression to de Bruijn index form, using the given context.
    The context is used to determine what index a variable should be
    associated with, and grows for recursive calls under binders.
 *)
let rec bruijnify_exp ctx = function
  | Var x ->
     begin
       match index_of x ctx with
       | Some i -> Var ("__" ^ string_of_int i)
       | None -> Var x
     end
  | I x -> I x
  | B x -> B x
  | If (e, e1, e2) ->
     If (bruijnify_exp ctx e, bruijnify_exp ctx e1, bruijnify_exp ctx e2)
  | Primop (op, es) -> Primop (op, List.map (bruijnify_exp ctx) es)
  | Fn (xs, e) ->
      let xs' = List.rev (List.map fst xs) in
      let underscores = List.map (fun (_, t) -> ("_", t)) xs in
      Fn (underscores, bruijnify_exp (xs' @ ctx) e)
  | Rec (x, t, e) ->
     Rec ("_", t, bruijnify_exp (x :: ctx) e)
  | Let (x, e1, e2) ->
      let e1' = bruijnify_exp ctx e1 in
      Let ("_", e1', bruijnify_exp (x :: ctx) e2)
  | Apply (e, es) ->
     Apply (bruijnify_exp ctx e, List.map (bruijnify_exp ctx) es)

let compare_exp e1 e2 =
  let e1' = bruijnify_exp [] e1 in
  let e2' = bruijnify_exp [] e2 in
  e1' = e2'

(* Graders *)
let test_fn_1
  ty name
  ?(mut_compare = (=)) muts
  ?(sol_compare = test) sol_tests =
  let mut_report =
    test_unit_tests_1 ty name
      ~test: mut_compare
      muts
  in
  let sol_report =
    test_function_1_against_solution ty name
      ~test: sol_compare
      ~gen: 0
      sol_tests
  in
  concat_reports mut_report sol_report

let test_fn_2
  ty name
  ?(mut_compare = (=)) muts
  ?(sol_compare = test) sol_tests =
  let mut_report =
    test_unit_tests_2 ty name
      ~test: mut_compare
      muts
  in
  let sol_report =
    test_function_2_against_solution ty name
      ~test: sol_compare
      ~gen: 0
      sol_tests
  in
  concat_reports mut_report sol_report

let sample_apply_args () =
  let args =
    if Random.bool () then
      [sample_simple_expr ();
       sample_let_with_unused ();
       sample_let_with_unused ()]
    else
      [sample_simple_expr ();
       sample_simple_expr ();
       sample_let_with_unused ()]
  in
  shuffle args

(* QUESTION 1: unused_vars *)

(* Mutants *)
let rec unused_vars_rec1 exp =
  match exp with
  | Rec (x, _, e) -> x :: unused_vars_rec1 e
  | _ -> Solution.unused_vars exp
let rec unused_vars_rec2 exp =
  match exp with
  | Rec (_, _, e) -> unused_vars_rec2 e
  | _ -> Solution.unused_vars exp
let rec unused_vars_fn1 exp =
  match exp with
  | Fn (_ :: _ :: _, _) -> raise NotImplemented
  | _ -> Solution.unused_vars exp
let rec unused_vars_fn2 exp =
  match exp with
  | Fn (_, e) -> unused_vars_fn2 e
  | _ -> Solution.unused_vars exp
let rec unused_vars_app exp =
  match exp with
  | Apply (_, _ :: _ :: _) -> raise NotImplemented
  | _ -> Solution.unused_vars exp

let sample_unused_vars_tests () =
  (* rec: bound variable is used *)
  let rec1 =
    let name = sample_name () in
    Rec (name, sample_simple_type (), Var name)
  in
  (* rec: bound variable is not used *)
  let rec2 =
    let (x1, x2) = sample_different_names () in
    Rec (x1, sample_simple_type (), Var x2)
  in
  (* rec: bound variable is shadowed *)
  let rec3 =
    let name = sample_name () in
    Rec (name, sample_simple_type (),
         Let (name, sample_simple_expr (), Var name))
  in
  (* rec: bound variable is used and also shadowed *)
  let rec4 =
    let name = sample_name () in
    Rec (name, Int,
         Let (name, sample_primop_expr (Var name), Var name))
  in
  (* rec: unused variable in sub-expression *)
  let rec5 =
    let (x1, x2) = sample_different_names () in
    Rec (x1, Int,
         Let (x2, sample_primop_expr (Var x1),
              sample_simple_expr ()))
  in
  (* fn: all bound variables are used *)
  let fn1 =
    let names = sample_three_names () in
    let bindings = List.map (fun x -> (x, Int)) names in
    let [x1; x2; x3] = shuffle names in
    Fn (bindings,
        sample_primop_exprs
          (Var x1)
          (sample_primop_exprs (Var x2) (Var x3)))
  in
  (* fn: at least one bound var used, at least one unused *)
  let fn2 =
    let names = sample_three_names () in
    let bindings = List.map (fun x -> (x, Int)) names in
    let [x1; x2; _] = shuffle names in
    let expr =
      if Random.bool () then
        sample_primop_expr (Var x1)
      else
        sample_primop_exprs (Var x1) (Var x2)
    in
    Fn (bindings, expr)
  in
  (* fn: no bound variables are used *)
  let fn3 =
    Fn (sample_bindings (), sample_simple_expr ())
  in
  (* fn: some bound variable is shadowed *)
  let fn4 =
    let ((x, _) :: _) as bindings = sample_bindings () in
    let bindings = shuffle bindings in
    Fn (bindings,
        Let (x, sample_simple_expr (), sample_primop_expr (Var x)))
  in
  (* fn: unused variable in sub-expression *)
  let fn5 =
    Fn (sample_bindings (), sample_let_with_unused ())
  in
  (* apply: no unused variables *)
  let app1 =
    Apply (Var (sample_name ()), sample_simple_exprs ())
  in
  (* apply: unused variable in function expression *)
  let app2 =
    let (x1, x2) = sample_different_names () in
    let exprs = sample_simple_exprs () in
    Apply (Let (x1, sample_simple_expr (), Var x2), exprs)
  in
  (* apply: unused variable in at least one argument *)
  let app3 =
    Apply (Var (sample_name ()), sample_apply_args ())
  in
  (* apply: unused variable in function expression and at least one arg *)
  let app4 =
    let (x1, x2) = sample_different_names () in
    Apply (Let (x1, sample_simple_expr (), Var x2), sample_apply_args ())
  in
  (* tail-recursive factorial with unnecessary rec flag and unused variable.
     If they've passed all the other tests, they should pass this one no
     problem unless they've done something very strange.
  *)
  let fact =
    Rec ("fact_tr", Arrow ([Int], Int),
      Fn ([("x", Int)],
        Let ("aux",
          Rec ("aux", Arrow ([Int; Int], Int),
            Fn ([("n", Int); ("acc", Int)],
              If (Primop (Equals, [Var "n"; I 0]),
                  Var "acc",
                  Let ("m", Primop (Minus, [Var "n"; I 1]),
                    Apply (Var "aux",
                           [Primop (Minus, [Var "n"; I 1]);
                            Primop (Times, [Var "n"; Var "acc"])]))))),
          Apply (Var "aux", [Var "x"; I 1]))))
  in
  [rec1; rec2; rec3; rec4; rec5;
   fn1; fn2; fn3; fn4; fn5;
   app1; app2; app3; app4;
   fact]

let test_unused_vars () =
  let tests = sample_unused_vars_tests () in
  let report =
    test_fn_1
      [%ty: exp -> name list] "unused_vars"
      ~mut_compare: compare_ignoring_order
      [("Rec: assumes variable is always unused", 1, unused_vars_rec1);
       ("Rec: assumes variable is always used", 1, unused_vars_rec2);
       ("Fn: fails on functions with more than one argument",
        1, unused_vars_fn1);
       ("Fn: assumes all variables are used", 1, unused_vars_fn2);
       ("Apply: fails when applying a function to more than one argument",
        1, unused_vars_app)]
      ~sol_compare: (test_canon_ok (List.sort compare))
      tests
  in
  Section ([Text "Question 1:"; Code "unused_vars"], report)

(* QUESTION 2: subst *)

let subst_rec1 s exp =
  match exp with
  | Rec _ -> exp
  | _ -> Solution.subst s exp
let subst_rec2 ((_, x) as s) exp =
  match exp with
  | Rec (y, t, e) when y = x ->  Rec (y, t, Solution.subst s e)
  | _ -> Solution.subst s exp
let subst_rec3 ((_, x) as s) exp =
  match exp with
  | Rec (y, t, e) when y <> x -> Rec (y, t, Solution.subst s e)
  | _ -> Solution.subst s exp
let subst_fn3 s exp =
  match exp with
  | Fn _ -> exp
  | _ -> Solution.subst s exp
let subst_fn1 ((_, x) as s) exp =
  match exp with
  | Fn (xs, e) when List.mem x (List.map fst xs) -> Fn (xs, Solution.subst s e)
  | _ -> Solution.subst s exp
let subst_fn2 ((_, x) as s) exp =
  match exp with
  | Fn (xs, e) when not (List.mem x (List.map fst xs)) ->
      Fn (xs, Solution.subst s e)
  | _ -> Solution.subst s exp
let subst_app1 s exp =
  match exp with
  | Apply _ -> exp
  | _ -> Solution.subst s exp
let subst_app2 s exp =
  match exp with
  | Apply (e, e' :: es) ->
      Apply (Solution.subst s e, Solution.subst s e' :: es)
  | _ -> Solution.subst s exp

let sample_subst_tests () =
  (* rec: x doesn't occur at all in the expression *)
  let rec1 =
    let [x; y; z] = sample_three_names () in
    let expr =
      Rec (y, sample_simple_type (), sample_primop_expr (Var z))
    in
    ((sample_simple_expr (), x), expr)
  in
  (* rec: general case (free occurrences, but no renaming) *)
  let rec2 =
    let [x; y; z] = sample_three_names () in
    let expr =
      Rec (y, sample_simple_type (), sample_primop_exprs (Var x) (Var x))
    in
    ((Var z, x), expr)
  in
  (* rec: y = x*)
  let rec3 =
    let x = sample_name () in
    let expr = Rec (x, sample_simple_type (), Var x) in
    ((sample_simple_expr (), x), expr)
  in
  (* rec: y occurs free in e' *)
  let rec4 =
    let (x, y) = sample_different_names () in
    let e' = sample_primop_expr (Var y) in
    let expr = Rec (y, Int, sample_primop_exprs (Var x) (Var y)) in
    ((e', x), expr)
  in
  (* fn: x doesn't occur at all in the expression *)
  let fn1 =
    let ((y, _) :: _) as bindings = sample_bindings () in
    let x = sample_different_name_binds bindings () in
    let expr = Fn (shuffle bindings, Var y) in
    ((sample_simple_expr (), x), expr)
  in
  (* fn: general case (free occurrences, but no renaming) *)
  let fn2 =
    let ((y, _) :: _) as bindings = sample_bindings () in
    let x = sample_different_name_binds bindings () in
    let expr = Fn (shuffle bindings, sample_primop_exprs (Var x) (Var y)) in
    ((sample_simple_expr (), x), expr)
  in
  (* fn: x is bound by fn *)
  let fn3 =
    let ((x, _) :: _) as bindings = sample_bindings ~min: 3 () in
    let expr = Fn (shuffle bindings, Var x) in
    ((sample_simple_expr (), x), expr)
  in
  (* fn: one of the xs appears free in e' *)
  let fn4 =
    let ((y, _) :: _) as bindings = sample_bindings ~min: 3 () in
    let x = sample_different_name_binds bindings () in
    let e' = sample_primop_expr (Var y) in
    let expr = Fn (shuffle bindings, sample_primop_exprs (Var x) (Var y)) in
    ((e', x), expr)
  in
  (* fn: multiple xs appear free in e' *)
  let fn5 =
    let [(y1, _); (y2, _); _] as bindings = sample_bindings ~min: 3 () in
    let x = sample_different_name_binds bindings () in
    let e' = sample_primop_exprs (Var y1) (Var y2) in
    let expr = Fn (shuffle bindings, sample_primop_exprs (Var x) (Var y1)) in
    ((e', x), expr)
  in
  (* apply: no free occurrences *)
  let app1 =
    let [x; y; z] = sample_three_names () in
    let args = [Var y; sample_simple_expr (); sample_simple_expr ()] in
    let expr = Apply (Var x, shuffle args) in
    ((sample_simple_expr (), z), expr)
  in
  (* apply: free occurrence in function expression *)
  let app2 =
    let [x; y; z] = sample_three_names () in
    let args = [Var y; sample_simple_expr (); sample_simple_expr ()] in
    let expr = Apply (Var x, shuffle args) in
    ((Var z, x), expr)
  in
  (* apply: free occurrences in arguments *)
  let app3 =
    let [x; y; z] = sample_three_names () in
    let args = [sample_primop_expr (Var x);
                sample_primop_exprs (Var y) (Var x);
                sample_simple_expr ()]
    in
    let expr = Apply (Var z, shuffle args) in
    ((sample_simple_expr (), x), expr)
  in
  (* apply: free occurrences in function expression and args *)
  let app4 =
    let [x; y; z] = sample_three_names () in
    let args = [sample_primop_expr (Var x);
                sample_primop_exprs (Var y) (Var x);
                sample_simple_expr ()]
    in
    let expr = Apply (Var x, shuffle args) in
    ((sample_simple_expr (), x), expr)
  in
  [rec1; rec2; rec3; rec4;
   fn1; fn2; fn3; fn4; fn5;
   app1; app2; app3; app4]

let test_subst () =
  let tests = sample_subst_tests () in
  let report =
    test_fn_2
      [%ty: (exp * name) -> exp -> exp] "subst"
      ~mut_compare: compare_exp
      [("Rec: general case", 1, subst_rec1);
       ("Rec: doesn't check if bound variable is the same as x", 1, subst_rec2);
       ("Rec: doesn't check if bound variable occurs free in e'", 1, subst_rec3);
       ("Fn: general case", 1, subst_fn3);
       ("Fn: doesn't check if any bound variable is the same as x", 1, subst_fn1);
       ("Fn: doesn't check if any bound variable occurs free in e'", 1, subst_fn2);
       ("Apply: general case", 1, subst_app1);
       ("Apply: only processes the first argument", 1, subst_app2)]
      ~sol_compare: (test_canon_ok (bruijnify_exp []))
      tests
  in
  let report = scale 2 report in
  Section ([Text "Question 2:"; Code "subst"], report)

(* Some more complicated test programs for eval and infer *)
let sum_up = Solution.sum_up
let fact_tr = Solution.fact_tr
let div = Solution.div
let is_even = Solution.is_even

let sample_single_arg_function () =
  let x = sample_name () in
  if Random.bool () then
    (Fn ([(x, Int)], sample_primop_expr (Var x)),
     sample_simple_int ())
  else
    (Fn ([(x, Bool)],
         If (Var x, sample_simple_bool (), sample_simple_bool ())),
     sample_simple_bool ())

(* QUESTION 3: eval *)

let eval_rec exp =
  match exp with
  | Rec _ -> exp
  | _ -> Solution.eval exp
let eval_app1 exp =
  match exp with
  | Apply (_, []) -> raise NotImplemented
  | _ -> Solution.eval exp
let eval_app2 exp =
  match exp with
  | Apply (_, [x]) -> raise NotImplemented
  | _ -> Solution.eval exp
let eval_app3 exp =
  match exp with
  | Apply (_, _ :: _ :: _) -> raise NotImplemented
  | _ -> Solution.eval exp
let eval_app4 exp =
  match exp with
  | Apply (Fn _, _) -> Solution.eval exp
  | Apply _ -> raise NotImplemented
  | _ -> Solution.eval exp
let eval_app5 exp =
  match exp with
  | Apply (e, es) ->
      begin
        match Solution.eval e with
        | Fn (_, e) -> Solution.eval e
        | _ -> raise (Stuck Apply_non_fn)
      end
  | _ -> Solution.eval exp

let sample_eval_tests () =
  (* rec: bound variable doesn't appear in expression *)
  let rec1 =
    Rec (sample_name (), sample_simple_type (),
         sample_primop_expr (sample_simple_int ()))
  in
  (* rec: perform a single substitution *)
  let rec2 =
    let (x, y) = sample_different_names () in
    Rec (x, sample_simple_type (),
         Fn ([(y, sample_simple_type ())], Var x))
  in
  (* apply: simplest possible function and argument *)
  let app1 =
    let name = sample_name ()  in
    let ty, arg =
      if Random.bool () then (Int, sample_simple_int ())
      else (Bool, sample_simple_bool ())
    in
    Apply (Fn ([(name, ty)], Var name), [arg])
  in
  (* apply: 0-argument function, no args *)
  let app2 =
    Apply (Fn ([], sample_simple_expr ()), [])
  in
  (* apply: single-argument fn and single arg, both already values *)
  let app3 =
    let (fn, arg) = sample_single_arg_function () in
    Apply (fn, [arg])
  in
  (* apply: multi-argument fn and correct number of args, all already values *)
  let app4 =
    let (x, y) = sample_different_names () in
    let fn = Fn ([(x, Int); (y, Int)], sample_primop_exprs (Var x) (Var y)) in
    Apply (fn, [sample_simple_int (); sample_simple_int ()])
  in
  (* apply: single-argument fn and single arg, fn is not a value *)
  let app5 =
    let (x, y) = sample_different_names () in
    let fn =
      Let (x, sample_simple_int (),
           Fn ([(y, Int)], sample_primop_exprs (Var x) (Var y)))
    in
    Apply (fn, [sample_simple_int ()])
  in
  (* apply: single-argument fn, too many arguments *)
  let app6 =
    let x = sample_name () in
    let fn = Fn ([(x, sample_simple_type ())], Var x) in
    Apply (fn, [sample_simple_expr (); sample_simple_expr ()])
  in
  (* apply: multi-argument fn, fn is not a value *)
  let app7 =
    let [x; y; z] = sample_three_names () in
    let fn =
      If (sample_simple_bool (),
          Fn ([(x, Int); (y, Int); (z, Int)],
              sample_primop_exprs (Var x) (Var y)),
          Fn ([(x, Int); (y, Int); (z, Int)],
              sample_primop_exprs (Var y) (Var z)))
    in
    Apply (fn, make_list sample_simple_int 3)
  in
  (* apply: all arguments need to be reduced *)
  let app8 =
    let [x; y; z] = sample_three_names () in
    let fn =
      Fn ([(x, Bool); (y, Int); (z, Int)],
          If (Var x,
              sample_primop_exprs (Var y) (Var z),
              sample_primop_exprs (Var y) (Var z)))
    in
    let arg1 =
      If (sample_simple_bool (), sample_simple_bool (), sample_simple_bool ())
    in
    Apply (fn, [arg1; sample_int_primop (); sample_int_primop ()])
  in
  (* apply: multi-argument fn, not enough arguments *)
  let app9 =
    let [(x, _); (y, _)] as bindings = sample_bindings ~min: 2 ~max: 2 () in
    let fn =
      Fn (bindings, if Random.bool() then Var x else Var y)
    in
    Apply (fn, [sample_simple_expr ()])
  in
  (* apply: multi-argument fn, too many arguments *)
  let app10 =
    let [(x, _); (y, _)] as bindings = sample_bindings ~min: 2 ~max: 2 () in
    let fn =
      Fn (bindings, if Random.bool () then Var x else Var y)
    in
    Apply (fn, make_list sample_simple_expr 3)
  in
  (* Misc. programs combining rec and apply.
     There isn't really any reason for these tests to fail if the above pass.
  *)
  let misc1 =
    Let ("sum_up", sum_up, Apply (Var "sum_up", [I (Random.int 10)]))
  in
  let misc2 =
    Apply (fact_tr, [I (Random.int 10)])
  in
  let misc3 =
    Let ("div", div,
         Apply (Var "div", [I (Random.int 10);
                            I (1 + Random.int 5)]))
  in
  let misc4 =
    Apply (is_even, [I (Random.int 10)])
  in
  [rec1; rec2;
   app1; app2; app3; app4; app5; app6; app7; app8; app9; app10;
   misc1; misc2; misc3; misc4]

let test_eval () =
  let tests = sample_eval_tests () in
  let report =
    test_fn_1
      [%ty: exp -> exp] "eval"
      ~mut_compare: compare_exp
      [("Rec: general case", 2, eval_rec);
       ("Apply: single-argument function", 2, eval_app2);
       ("Apply: function with more than one argument", 2, eval_app3);
       ("Apply: zero-argument function", 2, eval_app1);
       ("Apply: doesn't recursively evaluate the function expression", 2, eval_app4);
       ("Apply: doesn't substitute arguments into function body", 2, eval_app5)]
      ~sol_compare: (test_canon_ok (bruijnify_exp []))
      tests
  in
  Section ([Text "Question 3:"; Code "eval"], report)

(* QUESTION 4: infer *)

let infer_rec ctx exp =
  match exp with
  | Rec (_, _, e) -> Solution.infer ctx e
  | _ -> Solution.infer ctx exp
let infer_fn1 ctx exp =
  match exp with
  | Fn ([_], _) -> raise NotImplemented
  | _ -> Solution.infer ctx exp
let infer_fn2 ctx exp =
  match exp with
  | Fn (_ :: _ :: _, _) -> raise NotImplemented
  | _ -> Solution.infer ctx exp
let infer_fn3 ctx exp =
  match exp with
  | Fn ([], _) -> raise NotImplemented
  | _ -> Solution.infer ctx exp
let infer_app1 ctx exp =
  match exp with
  | Apply (_, [_]) -> raise NotImplemented
  | _ -> Solution.infer ctx exp
let infer_app2 ctx exp =
  match exp with
  | Apply (_, _ :: _ :: _) -> raise NotImplemented
  | _ -> Solution.infer ctx exp
let infer_app3 ctx exp =
  match exp with
  | Apply (_, []) -> raise NotImplemented
  | _ -> Solution.infer ctx exp

let sample_infer_tests () =
  (* rec: bound variable doesn't occur in body *)
  let rec1 =
    let x = sample_name () in
    ([], Rec (x, Int, sample_int_primop ()))
  in
  (* rec: bound variable occurs in body, simple *)
  let rec2 =
    let x = sample_name () in
    ([], Rec (x, sample_simple_type (), Var x))
  in
  (* rec: type mismatch, simple *)
  let rec3 =
    let x = sample_name () in
    let ty = sample_simple_type () in
    let body =
      match ty with
      | Bool -> sample_simple_int ()
      | _ -> sample_simple_bool ()
    in
    ([], Rec (x, ty, body))
  in
  (* rec: type mismatch, more complicated *)
  let rec4 =
    let x = sample_name () in
    let expr =
      Rec (x, Int,
           Primop (sample_cases [Equals; LessThan] (),
                   shuffle [Var x; sample_int_primop ()]))
    in
    ([], expr)
  in
  (* fn: bound variables not used in body *)
  let fn1 =
    ([], Fn (sample_bindings ~min: 2 (), sample_simple_expr ()))
  in
  (* fn: some bound variables used in body *)
  let fn2 =
    let [x; y; z] = sample_three_names () in
    let expr =
      if Random.bool () then sample_primop_exprs (Var x) (Var y)
      else sample_primop_exprs (Var y) (Var z)
    in
    ([], Fn ([(x, Int); (y, Int); (z, Int)], expr))
  in
  (* fn: zero-arity function *)
  let fn3 =
    ([], Fn ([], sample_simple_expr ()))
  in
  (* fn: no bound variables used, type mismatch *)
  let fn4 =
    ([], Fn (sample_bindings (), sample_primop_expr (sample_simple_bool ())))
  in
  (* fn: type mismatch because of a bound variable *)
  let fn5 =
    let (x, y) = sample_different_names () in
    ([], Fn ([(x, Int); (y, Bool)], sample_primop_exprs (Var x) (Var y)))
  in
  (* fn: nested fn with shadowed variable *)
  let fn6 =
    let ((x, t1) :: _) as bindings = sample_bindings ~min: 2 () in
    let t2 =
      let rec loop () =
        let ty = sample_simple_type () in
        if ty = t1 then loop () else ty
      in
      loop ()
    in
    ([], Fn (shuffle bindings, Fn ([(x, t2)], Var x)))
  in
  (* apply: simple *)
  let app1 =
    let f = sample_name () in
    ([(f, Arrow ([Int], Int))], Apply (Var f, [sample_simple_int ()]))
  in
  (* apply: zero-arity function *)
  let app2 =
    let f = sample_name () in
    ([(f, Arrow ([], sample_simple_type ()))], Apply (Var f, []))
  in
  (* apply: multi-argument function *)
  let app3 =
    let f = sample_name () in
    let (tys, args) =
      List.split
        (shuffle
          (make_list sample_atomic_type_and_value 2))
    in
    let ty = Arrow (tys, sample_simple_type ()) in
    let expr = Apply (Var f, args) in
    ([(f, ty)], expr)
  in
  (* apply: applying a non-function *)
  let app4 =
    ([], Apply (sample_simple_expr (), [sample_simple_expr ()]))
  in
  (* apply: function expression isn't a value *)
  let app5 =
    let [x; y; z] = sample_three_names () in
    let t1 = sample_atomic_type () in
    let t2 = sample_atomic_type () in
    let arr1 = Arrow ([t1], Arrow ([t2], sample_atomic_type ())) in
    let ctx = [(x, arr1); (y, t1); (z, t2)] in
    let expr = Apply (Apply (Var x, [Var y]), [Var z]) in
    (ctx, expr)
  in
  (* apply: argument type mismatch *)
  let app6 =
    let tys_and_args = make_list sample_atomic_type_and_value 2 in
    let mismatch =
      if Random.bool () then (Int, sample_simple_bool ())
      else (Bool, sample_simple_int ())
    in
    let tys_and_args = shuffle (mismatch :: tys_and_args) in
    let (tys, args) = List.split tys_and_args in
    let f = sample_name () in
    let ctx = [(f, Arrow (tys, sample_simple_type ()))] in
    let expr = Apply (Var f, args) in
    (ctx, expr)
  in
  (* apply: too few arguments *)
  let app7 =
    let (tys, args) =
      List.split @@ make_list sample_atomic_type_and_value 2
    in
    let tys = tys @ [sample_simple_type ()] in
    let f = sample_name () in
    let ctx = [(f, Arrow (tys, sample_simple_type ()))] in
    (ctx, Apply (Var f, args))
  in
  (* apply: too many arguments *)
  let app8 =
    let (tys, args) =
      List.split @@ make_list sample_atomic_type_and_value 2
    in
    let args = args @ [sample_simple_expr ()] in
    let f = sample_name () in
    let ctx = [(f, Arrow (tys, sample_simple_type ()))] in
    (ctx, Apply (Var f, args))
  in
  (* apply: recursing on sub-expressions *)
  let app9 =
    let (f, g) = sample_different_names () in
    let ctx =
      [(f, Arrow ([Int], Arrow ([Int; Int], sample_simple_type ())));
       (g, Arrow ([Int], Int))]
    in
    let expr =
      Apply (Apply (Var f, [sample_int_primop ()]),
             make_list (fun () -> Apply (Var g, [sample_int_primop ()])) 2)
    in
    (ctx, expr)
  in
  (* Misc. programs combining fn, rec, and apply.
     There isn't really any reason for these tests to fail if the above pass.
  *)
  let misc1 =
    ([], Let ("sum_up", sum_up, Apply (Var "sum_up", [I (Random.int 10)])))
  in
  let misc2 =
    ([], Apply (fact_tr, [I (Random.int 10)]))
  in
  let misc3 =
    ([],
     Let ("div", div,
          Apply (Var "div", [I (Random.int 10);
                             I (1 + Random.int 5)])))
  in
  let misc4 =
    ([], Apply (is_even, [I (Random.int 10)]))
  in
  [rec1; rec2; rec3; rec4;
   fn1; fn2; fn3; fn4; fn5; fn6;
   app1; app2; app3; app4; app5; app6; app7; app8; app9;
   misc1; misc2; misc3; misc4]

let test_infer () =
  let tests = sample_infer_tests () in
  let report =
    test_fn_2
      [%ty: context -> exp -> tp] "infer"
      [("Rec: doesn't add new variable to the context", 1, infer_rec);
       ("Fn: single-argument function", 1, infer_fn1);
       ("Fn: function with more than one argument", 1, infer_fn2);
       ("Fn: zero-argument function", 1, infer_fn3);
       ("Apply: single argument", 1, infer_app1);
       ("Apply: more than one argument", 1, infer_app2);
       ("Apply: no arguments", 1, infer_app3)]
      tests
  in
  let report = scale 2 report in
  Section ([Text "Question 4:"; Code "infer"], report)

(* let test_unify () = *)
(*   let trap f = *)
(*     try *)
(*       f (); *)
(*       true *)
(*     with *)
(*     | _ -> false *)
(*   in *)
(*   let valid = *)
(*     [ *)
(*       (let x = fresh_tvar () in (x, x)); *)
(*       (let x = fresh_tvar () in let y = fresh_tvar () in (x, y)); *)
(*       (let x = fresh_tvar () in (x, UInt)) *)
(*     ] *)
(*   in *)
(*   let invalid = *)
(*     [ *)
(*       (UInt, UBool); *)
(*       (UTVar (ref (Some (UArrow (UInt, UInt)))), UTVar (ref (Some UInt))) *)
(*     ] *)
(*   in *)
(*   let report = load_student [%ty: utp -> utp -> unit] "unify" @@ *)
(*     fun sf -> *)
(*     let valid_passed = List.length (List.filter (fun (t1, t2) -> trap (fun () -> sf t1 t2)) valid) in *)
(*     let invalid_passed = List.length (List.filter (fun (t1, t2) -> not (trap (fun () -> sf t1 t2))) invalid) in *)
(*     let total = List.length valid + List.length invalid in *)
(*     let succeeded = valid_passed + invalid_passed in *)
(*     if succeeded >= total then *)
(*       Message ([Text (Format.sprintf "You passed %d hidden test(s)" succeeded)], Success 1) *)
(*     else *)
(*       Message ([Text (Format.sprintf "You failed %d hidden test(s)" (total - succeeded))], Failure) *)
(*   in *)
(*   Section ([Text "Question 5:"; Code "unify"], report) *)

let () =
  set_result @@ ast_sanity_check code_ast @@ fun () ->
    (* Enforce evaluation order *)
    set_progress "Grading unused_vars";
    let q1 = test_unused_vars () in
    set_progress "Grading subst";
    let q2 = test_subst () in
    set_progress "Grading eval";
    let q3 = test_eval () in
    set_progress "Grading infer";
    let q4 = test_infer () in
    (* set_prrogress "Grading unify"; *)
    (* let q5 = test_unify () in *)
    [q1; q2; q3; q4]
