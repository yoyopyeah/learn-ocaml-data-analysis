(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* rec: bound variable is used *)
  (Rec ("f", Int, Var "f"), []);
  (* rec: bound variable is unused *)
  (Rec ("f", Int, Var "g"), ["f"]);
  (* rec: bound variable is shadowed *)
  (Rec ("f", Int,
        Let ("f", I 1, Var "f")),
   ["f"]);
  (* rec: bound variable is used and also shadowed *)
  (Rec ("f", Int,
        Primop (Plus, [Var "f";
                       Let ("f", I 1, Var "f")])),
   []);
  (* rec: unused variable in sub-expression *)
  (Rec ("f", Int, Let ("x", I 1, Var "f")), ["x"]);
  (* fn: all bound variables are used *)
  (Fn ([("x", Int); ("y", Int); ("z", Int)],
       Primop (Plus, [Var "x";
                      Primop (Plus, [Var "y";
                                     Var "z"])])),
   []);
  (* fn: some bound variables unused *)
  (Fn ([("x", Int); ("y", Int); ("z", Int)],
       Primop (Plus, [Var "x";
                      Var "y"])),
   ["z"]);
  (* fn: all bound variables unused *)
  (Fn ([("x", Int); ("y", Int); ("z", Int)], I 0), ["x"; "y"; "z"]);
  (* fn: some bound variables shadowed *)
  (Fn ([("x", Int); ("y", Int)],
       Let ("y", Var "x", Var "y")),
   ["y"]);
  (* fn: unused variable in sub-expression *)
  (Fn ([("x", Int)],
       Let ("y", Var "x", I 0)),
   ["y"]);
  (* apply: unused variable in function expression *)
  (Apply (Let ("y", I 1, Var "f"),
          [I 0; I 1]),
   ["y"]);
  (* apply: unused variable in arguments *)
  (Apply (Var "f",
          [I 0;
           Let ("x", I 1, I 2);
           Let ("y", I 1, Var "y")]),
   ["x"]);
  (* apply: unused variable in function expression and arguments *)
  (Apply (Let ("y", I 1, Var "f"),
         [I 0;
          Let ("x", I 1, I 2);
          Let ("y", I 1, Var "y")]),
   ["y"; "x"])
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars = function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused

  | Rec (x, _, e) ->
      let e_vars = unused_vars e in
      if List.mem x (free_variables e) then
        e_vars
      else
        x :: e_vars

  | Fn (xs, e) ->
      let names = List.map fst xs in
      let unused = unused_vars e in
      let unused_xs = delete (free_variables e) names in
      unused @ unused_xs

  | Apply (e, es) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] (e :: es)

(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* rec: x doesn't occur in expression *)
  (((Var "y", "x"), (* [y/x] *)
    (* rec (f: int -> int) => f *)
    Rec ("f", Arrow ([Int], Int), Var "f")),
   (* expected: rec (f: int -> int) => f *)
   Rec ("f", Arrow ([Int], Int), Var "f"));

  (* rec: general case *)
  (((Var "y", "x"), (* [y/x] *)
    (* rec (z: int) => x + x + z *)
    Rec ("z", Int,
         Primop (Plus, [Var "x";
                        Primop (Plus, [Var "x"; Var "z"])]))),
   (* expected: rec (z: int) => y + y + z *)
   Rec ("z", Int,
        Primop (Plus, [Var "y";
                       Primop (Plus, [Var "y"; Var "z"])])));

  (* rec: x is bound by rec *)
  (((Var "y", "x"), (* [y/x] *)
    (* rec (x: int) => x *)
    Rec ("x", Int, Var "x")),
    (* expected: rec (x: int) => x *)
   Rec ("x", Int, Var "x"));

  (* rec: renaming required *)
  (((Var "y", "x"), (* [y/x] *)
    (* rec (y: int) => x + y *)
    Rec ("y", Int, Primop (Plus, [Var "x"; Var "y"]))),
   (* expected: rec (y1: int) => y + y1 *)
   Rec ("y1", Int, Primop (Plus, [Var "y"; Var "y1"])));

  (* fn: x doesn't occur in expression *)
  (((Var "y", "x"), (* [y/x] *)
    (* fun (a: int, b: int, c: int => a + b + c *)
    Fn ([("a", Int); ("b", Int); ("c", Int)],
        Primop (Plus, [Var "a";
                       Primop (Plus, [Var "b"; Var "c"])]))),
   (* expected: fun (a: int, b: int, c: int => a + b + c *)
   Fn ([("a", Int); ("b", Int); ("c", Int)],
        Primop (Plus, [Var "a";
                       Primop (Plus, [Var "b"; Var "c"])])));

  (* fn: general case *)
  (((Var "y", "x"), (* [y/x] *)
    (* fun (a: int, b: int, c: int) => x + x + a *)
    Fn ([("a", Int); ("b", Int); ("c", Int)],
        Primop (Plus, [Var "x";
                       Primop (Plus, [Var "x"; Var "a"])]))),
   (* expected: fun (a: int, b: int, c: int => y + y + a *)
   Fn ([("a", Int); ("b", Int); ("c", Int)],
       Primop (Plus, [Var "y";
                      Primop (Plus, [Var "y"; Var "a"])])));

  (* fn: x is bound by the function *)
  (((Var "y", "x"), (* [y/x] *)
    (* fun (w: int, x: int, z: int) => x *)
    Fn ([("w", Int); ("x", Int); ("z", Int)], Var "x")),
   (* expected: fun (w: int, x: int, z: int) => x *)
   Fn ([("w", Int); ("x", Int); ("z", Int)], Var "x"));

  (* fn: one of the bound variables appears free in e' *)
  (((Primop (Plus, [Var "y"; I 1]), "x"), (* [y + 1/x] *)
    (* fun (w: int, y: int, z: int) => x + y + z *)
    Fn ([("w", Int); ("y", Int); ("z", Int)],
        Primop (Plus, [Var "x";
                       Primop (Plus, [Var "y"; Var "z"])]))),
   (* expected: fun (w1: int, y1: int, z1: int) => (y + 1) + y1 + z1 *)
   Fn ([("w1", Int); ("y1", Int); ("z1", Int)],
       Primop (Plus, [Primop (Plus, [Var "y"; I 1]);
                      Primop (Plus, [Var "y1"; Var "z1"])])));

  (* fn: multiple bound variables appear free in e' *)
  (((Primop (Plus, [Var "y"; Var "z"]), "x"), (* [y + z/x] *)
    (* fun (y: int, z: int) => y * (z + x) *)
    Fn ([("y", Int); ("z", Int)],
        Primop (Times, [Var "y";
                        Primop (Plus, [Var "z"; Var "x"])]))),
   (* expected: fun (y1: int, z1: int) => y * (z1 + y + z) *)
   Fn ([("y1", Int); ("z1", Int)],
       Primop (Times, [Var "y1";
                       Primop (Plus, [Var "z1";
                                      Primop (Plus, [Var "y"; Var "z"])])])));

  (* apply: x doesn't occur in expression *)
  (((Var "y", "x"), (* [y/x] *)
    (* f a b *)
    Apply (Var "f", [Var "a"; Var "b"])),
   (* expected: f a b *)
   Apply (Var "f", [Var "a"; Var "b"]));

  (* apply: x occurs free in the function expression *)
  (((Var "y", "x"), (* [y/x] *)
    (* x a b *)
    Apply (Var "x", [Var "a"; Var "b"])),
   (* expected: y a b *)
   Apply (Var "y", [Var "a"; Var "b"]));

  (* apply: x occurs free in one of the arguments *)
  (((Var "y", "x"), (* [y/x] *)
    (* f x y *)
    Apply (Var "f", [Var "x"; Var "y"])),
   (* expected: f y y *)
   Apply (Var "f", [Var "y"; Var "y"]));

  (* apply: x occurs free in multiple arguments *)
  (((Var "y", "x"), (* [y/x] *)
    (* f x x x *)
    Apply (Var "f", [Var "x"; Var "x"; Var "x"])),
   (* expected: f y y y *)
   Apply (Var "f", [Var "y"; Var "y"; Var "y"]));

  (* apply: x occurs free in the function expression and arguments *)
  (((Var "y", "x"), (* [y/x] *)
    (* (f x) x y x *)
    Apply (Apply (Var "f", [Var "x"]),
          [Var "x"; Var "y"; Var "x"])),
    (* expected: (f y) y y y *)
    Apply (Apply (Var "f", [Var "y"]),
          [Var "y"; Var "y"; Var "y"]))
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) ->
      if y = x then
        Rec (y, t, e)
      else
        let (y, e) =
          if List.mem y (free_variables e') then
            rename y e
        else
          (y, e)
        in
        Rec (y, t, subst s e)

  | Fn (xs, e) ->
      let names = List.map fst xs in
      (* If the variable we're substituting for is bound by one of the
         xs, we can just stop recursing here.
      *)
      if List.mem x names then
        Fn (xs, e)
      else
        (* If we need to do a renaming, we can rename all variables at
           once and then recurse into the body of the function. *)
        let e'_vars = free_variables e' in
        let (xs, e) =
          if List.exists (fun y -> List.mem y e'_vars) names then
            let (names, e') = rename_all names e in
            let xs' = List.map2 (fun y (_, t) -> (y, t)) names xs in
            (xs', e')
          else
            (xs, e)
        in
        Fn (xs, subst s e)

  | Apply (e, es) -> Apply (subst s e, List.map (subst s) es)

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
      let (name', exp') = rename name exp in
      (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs


(* TODO: Write a good set of tests for eval. *)
let sum_up =
  Rec ("f", Arrow ([Int], Int),
    Fn ([("x", Int)],
        If (Primop (Equals, [Var "x"; I 0]),
            I 0,
            Primop (Plus,
                    [Apply (Var "f", [Primop (Minus, [Var "x"; I 1])]);
                     Var "x"]))))
let fact_tr =
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
let div =
  Rec ("div", Arrow ([Int; Int], Int),
    Fn ([("n", Int); ("m", Int)],
        Let ("d", Primop (Minus, [Var "n"; Var "m"]),
             If (Primop (LessThan, [Var "n"; Var "m"]),
                 I 0,
                 Let ("q", Apply (Var "div", [Var "d"; Var "m"]),
                      Primop (Plus, [Var "q"; I 1]))))))
let is_even =
  Rec ("f", Arrow ([Int], Bool),
    Fn ([("n", Int)],
        If (Primop (Equals, [Var "n"; I 0]),
            B true,
            If (Apply (Var "f", [Primop (Minus, [Var "n"; I 1])]),
                B false,
                B true))))

let eval_tests = [
  (* rec: bound variable doesn't appear in body *)
  (Rec ("f", Arrow ([Int], Int), Primop (Plus, [I 1; I 2])), I 3);
  (* rec: perform a single substitution *)
  (Rec ("f", Arrow ([Int], Int), Fn ([], Var "f")),
   Fn ([], Rec ("f", Arrow ([Int], Int), Fn ([], Var "f"))));
  (* apply: zero-argument function, no arguments *)
  (Apply (Fn ([], I 3), []), I 3);
  (* apply: single-argument fn, single arg, all already reduced *)
  (Apply (Fn ([("x", Int)], Var "x"), [I 4]), I 4);
  (* apply: function needs to be reduced *)
  (Apply (
      Let ("x", I 1,
           Fn ([("y", Int)], Primop (Plus, [Var "x"; Var "y"]))),
      [I 2]),
   I 3);
  (* apply: arguments need to be reduced *)
  (Apply (
      Fn ([("x", Int); ("y", Int)], Primop (LessThan, [Var "x"; Var "y"])),
      [Primop (Plus, [I 1; I 2]); Primop (Times, [I 3; I 4])]),
   B true);

  (* some combinations of rec and apply *)
  (Let ("sum_up", sum_up, Apply (Var "sum_up", [I 5])), I 15);
  (Apply (fact_tr, [I 4]), I 24);
  (Let ("div", div, Apply (Var "div", [I 8; I 2])), I 4);
  (Apply (is_even, [I 5]), B false)
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
  *)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, _, e) -> eval (subst (exp, f) e)

  | Apply (e, es) ->
      begin
        match eval e with
        | Fn (xs, e) ->
            if List.length xs = List.length es then
              let es = List.map eval es in
              let xs = List.map fst xs in
              let subs = List.combine es xs in
              eval (subst_list subs e)
            else
              raise (Stuck Arity_mismatch)

        | _ -> raise (Stuck Apply_non_fn)
      end

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* rec: bound variable doesn't occur in body *)
  (([], Rec ("f", Int, I 0)), Int);
  (* rec: bound variable appears in body, simple *)
  (([], Rec ("f", Arrow ([Int], Int), Var "f")), Arrow ([Int], Int));
  (* fn: bound variables not used in body *)
  (([], Fn ([("x", Int); ("y", Bool); ("z", Int)], I 0)),
   Arrow ([Int; Bool; Int], Int));
  (* fn: bound variables used in body *)
  (([], Fn ([("b", Bool); ("x", Int); ("y", Int)],
            If (Var "b",
                Primop (Plus, [Var "x"; Var "y"]),
                Primop (Minus, [Var "x"; Var "y"])))),
   Arrow ([Bool; Int; Int], Int));
  (* fn: zero-arity function *)
  (([], Fn ([], B true)), Arrow ([], Bool));
  (* fn: nested fn with shadowed variable *)
  (([], Fn (["x", Int], Fn (["x", Bool], Var "x"))),
   Arrow ([Int], Arrow ([Bool], Bool)));
  (* apply: simple *)
  (([("f", Arrow ([Int], Int))],
    Apply (Var "f", [I 0])),
   Int);
  (* apply: zero-arity function *)
  (([("f", Arrow ([], Bool))],
    Apply (Var "f", [])),
   Bool);
  (* apply: multi-argument function *)
  (([("f", Arrow ([Int; Bool; Int], Bool))],
    Apply (Var "f", [I 0; B true; I 1])),
   Bool);
  (* apply: function expression that isn't a value *)
  (([("f", Arrow ([Int], Arrow ([Bool], Bool)))],
    Apply (Apply (Var "f", [I 3]), [B true])),
   Bool);
  (* apply: recursing on sub-expressions *)
  (([("f", Arrow ([Int], Arrow ([Bool; Bool], Bool)));
     ("g", Arrow ([Int], Bool))],
    Apply (Apply (Var "f", [I 5]),
           [Apply (Var "g", [I 6]);
            Apply (Var "g", [I 7])])),
   Bool);

  (* some combinations of fn, rec, and apply *)
  (([], Let ("sum_up", sum_up, Apply (Var "sum_up", [I 5]))), Int);
  (([], Apply (fact_tr, [I 4])), Int);
  (([], Let ("div", div,
             Apply (Var "div", [I 6; I 3]))),
   Int);
  (([], Apply (is_even, [I 7])), Bool)
]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) ->
      let ctx' = extend ctx (f, t) in
      let t' = infer ctx' e in
      if t' = t then t
      else type_mismatch t t'

  | Fn (xs, e) ->
      let ctx' = extend_list ctx xs in
      let ts = List.map snd xs in
      Arrow (ts, infer ctx' e)

  | Apply (e, es) ->
      begin
        match infer ctx e with
        | Arrow (ts, t) -> check ctx es ts t
        | t' -> raise (TypeError (Apply_non_arrow t'))
      end

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)


(* Unification *)
let rec unify (t1 : utp) (t2 : utp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | UInt, UInt
  | UBool, UBool -> ()
  (* For type constructors, recursively unify the parts *)
  | UArrow (t1, t1'), UArrow (t2, t2') ->
    (unify t1 t2; unify t1' t2')
  | UTVar a, _ -> unifyVar a t2
  | _, UTVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t = match !a with
  | Some t1' -> unify t1' t
  | None ->
    match t with
    | UTVar b -> (match !b with
        | None -> if a != b then a := Some (UTVar b)
        | Some t2' -> unifyVar a t2')
    | _ ->
      if occurs a t then unif_error UnifOccursCheckFails
      else a := Some t
