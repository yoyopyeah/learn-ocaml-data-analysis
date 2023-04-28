(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
  ("5;", Right (Int 5))
  (* Provide your tests for the parser *)
]

(* Do not change this implementation. It is need for [parse_tests] to work. *)
let parse = P.parse

(* Q1  : Find the free variables in an expression *)
let bound_vars (d : dec) =
  match d with
  | Val (_, name) -> [name]
  | Valtuple (_, names) -> names
  | ByName (_, name) -> [name]

let bound_exp (d : dec) : exp =
  match d with
  | Val (e, _) -> e
  | Valtuple (e, _) -> e
  | ByName (e, _) -> e

(* free_vars (e) = list of names occurring free in e

   Invariant: every name occurs at most once. *)
let rec free_vars (e : exp) : name list =
  match e with
  | Var y -> [y]
  | Int _ | Bool _ -> []
  | If (e1, e2, e3) -> union_list [free_vars e1; free_vars e2; free_vars e3]
  | Primop (_, es) | Tuple es -> union_list (List.map free_vars es)
  | Fn (x, _, e) | Rec (x, _, e) -> delete [x] (free_vars e)
  | Let ([], e) ->
     free_vars e
  | Let (d :: ds, e) ->
     union (free_vars (bound_exp d))
           (delete (bound_vars d) (free_vars (Let (ds, e))))
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Anno (e, _) -> free_vars e

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list =
  match e with
  | Var _ | Int _ | Bool _ -> []
  | If (e1, e2, e3) -> union_list [unused_vars e1; unused_vars e2; unused_vars e3]
  | Primop (_, es) | Tuple es -> union_list (List.map unused_vars es)
  | Anno (e, _) -> unused_vars e

  | Let ([], e) ->
     unused_vars e
  | Let (d :: ds, e) ->
     let sub = Let (ds, e) in
     union_list [unused_vars (bound_exp d);
                 unused_vars sub;
                 delete (free_vars sub) (bound_vars d)
                ]

  | Apply (e1, e2) ->
     union (unused_vars e1) (unused_vars e2)
  | Fn (x, _, e) ->
     let free = free_vars e in
     union (unused_vars e) (delete free [x])
  | Rec (x, _, e) ->
     unused_vars e

(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) =
  match e with
  | Var y ->
     if x = y then
       e'
     else
       Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e, e1, e2) -> If (subst (e', x) e, subst (e', x) e1, subst (e', x) e2)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)

  | Let ([], e) -> Let ([], subst (e', x) e)
  | Let (d :: ds, e) ->
     let rest = Let (ds, e) in
     begin match d with
     | Val (ey, y) ->
        let ey = subst (e', x) ey in
        if y = x then
          Let (Val (ey, y) :: ds, e)
        else
          let (y, rest) =
            if member y (free_vars e') then
              rename (y, rest)
            else
              (y, rest)
          in
          begin match subst (e', x) rest with
          | Let (ds, e) -> Let (Val (ey, y) :: ds, e)
          | _ -> stuck "Invalid subst result : subst Let generates non-Let"
          end
     | ByName (ey, y) ->
        let ey = subst (e', x) ey in
        if y = x then
          Let (ByName (ey, y) :: ds, e)
        else
          let (y, rest) =
            if member y (free_vars e') then
              rename (y, rest)
            else
              (y, rest)
          in
          begin match subst (e', x) rest with
          | Let (ds, e) -> Let (ByName (ey, y) :: ds, e)
          | _ -> stuck "Invalid subst result : subst Let generates non-Let"
          end
     | Valtuple (eys, ys) ->
        let eys = subst (e', x) eys in
        if member x ys then
          Let (Valtuple (eys, ys) :: ds, e)
        else
          let (ys, rest) =
            let e'fv = free_vars e' in
            if List.exists (fun name -> member name e'fv) ys then
              rename_all (ys, rest)
            else
              (ys, rest)
          in
          begin match subst (e', x) rest with
          | Let (ds, e) -> Let (Valtuple (eys, ys) :: ds, e)
          | _ -> stuck "Invalid subst result : subst Let generates non-Let"
          end
     end

  | Apply (e1, e2) -> Apply (subst (e', x) e1, subst (e', x) e2)
  | Fn (y, t, e) ->
     if y = x then
       Fn (y, t, e)
     else if member y (free_vars e') then
       let (y, e) = rename (y, e) in
       Fn (y, t, subst (e', x) e)
     else
       Fn (y, t, subst (e', x) e)
  | Rec (f, t, e) ->
     if f = x then
       Rec (f, t, e)
     else if member f (free_vars e') then
       let (f, e) = rename (f, e) in
       Rec (f, t, subst (e', x) e)
     else
       Rec (f, t, subst (e', x) e)

and rename ((x, e) : name * exp) : name * exp =
  let x' = fresh_var x in
  (x', subst (Var x', x) e)

and rename_all ((xs, e) : name list * exp) : name list * exp =
  match xs with
  | [] -> ([], e)
  | x :: xs ->
     let (x, e) = rename (x, e) in
     let (xs, e) = rename_all (xs, e) in
     (x :: xs, e)


(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  let bigstep_depth = ref 0 in
  fun e ->
  if !debug >= 1 then
    print_endline
      (String.make (!bigstep_depth) ' '
       ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
  incr bigstep_depth;
  let result =
    match e with
    | Int _ | Bool _ -> e
    | Tuple es -> Tuple (List.map eval es)
    | If (e1, e2, e3) ->
       begin match eval e1 with
       | Bool b ->
          if b then
            eval e2
          else
            eval e3
       | _ -> stuck "Condition for if expression should be of the type Bool"
       end
    | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
    | Var x -> stuck ("Free variable (" ^ x ^ ") during evaluation")

    | Fn _ -> e
    | Apply (e1, e2) ->
       begin match eval e1 with
       | Fn (x, _, e1) -> eval (subst (e2, x) e1)
       | _ -> stuck "Left term of application is not an Fn"
       end
    | Rec (f, _, e') -> eval (subst (e, f) e')

    (* implements short circuiting *)
    | Primop (And, es) ->
       begin match es with
       | [e1; e2] ->
          begin match eval e1 with
          | Bool b ->
             if b then
               eval e2
             else
               Bool false
          | _ -> stuck "Bad arguments to primitive operation"
          end
       | _ -> stuck "Bad arguments to primitive operation"
       end
    | Primop (Or, es) ->
       begin match es with
       | [e1; e2] ->
          begin match eval e1 with
          | Bool b ->
             if b then
               Bool true
             else
               eval e2
          | _ -> stuck "Bad arguments to primitive operation"
          end
       | _ -> stuck "Bad arguments to primitive operation"
       end

    (* primitive operations +, -, *, <, = *)
    | Primop (po, es) ->
       let vs = List.map eval es in
       begin match eval_op po vs with
       | None -> stuck "Bad arguments to primitive operation"
       | Some v -> v
       end

    | Let ([], e) -> eval e
    | Let (d :: ds, e) ->
       begin match d with
       | Val (ex, x) ->
          (* call-by-value *)
          let vx = eval ex in
          eval (subst (vx, x) (Let (ds, e)))
       | Valtuple (exs, xs) ->
          begin match eval exs with
          | Tuple es ->
             if List.length es = List.length xs then
               eval (List.fold_right subst (List.combine es xs) (Let (ds, e)))
             else
               stuck "Tuple binding failure (length mismatch)"
          | _ ->
             stuck "Tuple binding failure"
          end
       | ByName (ex, x) -> (* call-by-name *)
          eval (subst (ex, x) (Let (ds, e)))
       end
  in
  (* do not change the code from here *)
  decr bigstep_depth;
  if !debug >= 1 then
    print_endline
      (String.make (!bigstep_depth) ' '
       ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
       ^ Print.exp_to_string result ^ "\n");
  (* to here *)
  result

(* Q5  : Type an expression *)
let rec infer (ctx : context) (e : exp) : typ =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | If (e1, e2, e3) ->
     if not (typ_eq (infer ctx e1) TBool) then
       type_fail "condition of if should have type bool";
     let t2 = infer ctx e2 in
     let t3 = infer ctx e3 in
     if not (typ_eq t2 t3) then
       type_fail "branches of if should have the same type";
     t2
  | Primop (op, es) ->
     begin match op with
     | LessThan | GreaterThan | LessEqual | GreaterEqual
       | Equals | NotEquals ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer ctx e1 in
           let t2 = infer ctx e2 in
           if typ_eq t1 TInt && typ_eq t2 TInt then
             TBool
           else
             type_fail "operator should take two integers"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | Plus | Minus | Times | Div ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer ctx e1 in
           let t2 = infer ctx e2 in
           if typ_eq t1 TInt && typ_eq t2 TInt then
             TInt
           else
             type_fail "operator should take two integers"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | And | Or ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer ctx e1 in
           let t2 = infer ctx e2 in
           if typ_eq t1 TBool && typ_eq t2 TBool then
             TBool
           else
             type_fail "logic operator should take two boolean"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | Negate ->
        begin match es with
        | [e] ->
           let t = infer ctx e in
           if typ_eq t TInt then
             TInt
           else
             type_fail "operator should take an integer"
        | _ -> type_fail "negate. shouldn't hit this case due to the parser"
        end
     end
  | Tuple es ->
     TProduct (List.map (infer ctx) es)
  | Fn (_, None, _) ->
     type_fail ("Can't infer type of expression " ^ Print.exp_to_string e)
  | Fn (x, Some t, e) ->
     let et = infer (extend ctx (x, t)) e in
     TArrow (t, et)
  | Rec (f, t, e) ->
     let et = infer (extend ctx (f, t)) e in
     if typ_eq et t then
       t
     else
       type_fail "recursive function does not have the ascribed type"
  | Let (ds, exp) ->
    infer (infer_bind ctx ds) exp
  | Apply (e1, e2) ->
     begin match infer ctx e1 with
     | TArrow (domain, range) ->
        if typ_eq (infer ctx e2) domain then
          range
        else
          type_fail (Print.exp_to_string e2 ^ " is expected to have a particular type")
     | _ -> type_fail ("Application expected arrow type for " ^ Print.exp_to_string e1)
     end
  (* this line is very strange. if we rename the function to [lookup] then learnocaml will fail *)
  | Var x -> ctx_lookup ctx x
  | Anno (e, t) ->
     if typ_eq (infer ctx e) t then
       t
     else
       type_fail (Print.exp_to_string e ^ " does not have type a particular type")

and infer_bind (ctx : context) (ds : dec list) =
  match ds with
  | [] -> ctx
  | (Val (ex, x) | ByName (ex, x)) :: ds ->
     infer_bind (extend ctx (x, infer ctx ex)) ds
  | Valtuple (exs, xs) :: ds ->
     begin match infer ctx exs with
     | TProduct ts ->
        if List.length ts <> List.length xs then
          type_fail "tuple unpack should have the same length as the type on the right hand side";
        infer_bind (extend_list ctx (List.combine xs ts)) ds
     | _ -> type_fail (Print.exp_to_string exs ^ " is expected to have a product type")
     end

(* Q6  : Unify two types *)
let rec find_root (t : typ) : typ =
  match t with
  | TVar r ->
     begin match !r with
     | None -> t
     | Some t ->
        let t' = find_root t in
        r := Some t';
        t'
     end
  | _ -> t

let rec self_referential (r : typ option ref) (t : typ) : bool =
  let t = find_root t in
  match t with
  | TInt | TBool -> false
  | TVar r' -> r == r'
  | TProduct ts -> List.exists (self_referential r) ts
  | TArrow (d, c) -> self_referential r d || self_referential r c

let rec unify (t1 : typ) (t2 : typ) : unit =
  let t1 = find_root t1 in
  let t2 = find_root t2 in
  match (t1, t2) with
  | (TInt, TInt)
    | (TBool, TBool) -> ()
  | (TProduct ts1, TProduct ts2) ->
     if List.length ts1 <> List.length ts2 then
       type_fail "cannot be unified because they have different length";
     List.iter2 unify ts1 ts2
  | (TArrow (dom1, cod1), TArrow (dom2, cod2)) ->
     unify dom1 dom2;
     unify cod1 cod2
  | (TVar r1, TVar r2) when r1 == r2 -> ()
  | (TVar r1, _) ->
     begin match !r1 with
     | None ->
        if self_referential r1 t2 then
          type_fail "a type variable is unified to a type referring to itself";
        r1 := Some t2
     | Some t1 -> unify t1 t2
     end
  | (_, TVar r2) ->
     begin match !r2 with
     | None ->
        if self_referential r2 t1 then
          type_fail "a type variable is unified to a type referring to itself";
        r2 := Some t1
     | Some t2 -> unify t1 t2
     end
  | _ -> type_fail "cannot be unified"

(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
let typ_unify_eq (t1 : typ) (t2 : typ) : bool =
  try
    unify t1 t2;
    true
  with
  | TypeError _ -> false

let rec infer' (ctx : context) (e : exp) : typ =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | If (e1, e2, e3) ->
     if not (typ_unify_eq (infer' ctx e1) TBool) then
       type_fail "condition of if should have type bool";
     let t2 = infer' ctx e2 in
     let t3 = infer' ctx e3 in
     if not (typ_unify_eq t2 t3) then
       type_fail "branches of if should have the same type";
     t2
  | Primop (op, es) ->
     begin match op with
     | LessThan | GreaterThan | LessEqual | GreaterEqual
       | Equals | NotEquals ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer' ctx e1 in
           let t2 = infer' ctx e2 in
           if typ_unify_eq t1 TInt && typ_unify_eq t2 TInt then
             TBool
           else
             type_fail "operator should take two integers"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | Plus | Minus | Times | Div ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer' ctx e1 in
           let t2 = infer' ctx e2 in
           if typ_unify_eq t1 TInt && typ_unify_eq t2 TInt then
             TInt
           else
             type_fail "operator should take two integers"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | And | Or ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer' ctx e1 in
           let t2 = infer' ctx e2 in
           if typ_unify_eq t1 TBool && typ_unify_eq t2 TBool then
             TBool
           else
             type_fail "logic operator should take two boolean"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | Negate ->
        begin match es with
        | [e] ->
           let t = infer' ctx e in
           if typ_unify_eq t TInt then
             TInt
           else
             type_fail "operator should take an integer"
        | _ -> type_fail "negate. shouldn't hit this case due to the parser"
        end
     end
  | Tuple es ->
     TProduct (List.map (infer' ctx) es)
  | Fn (x, None, e) ->
     let t = fresh_tvar () in
     let et = infer' (extend ctx (x, t)) e in
     let t = find_root t in
     TArrow (t, et)
  | Fn (x, Some t, e) ->
     let et = infer' (extend ctx (x, t)) e in
     TArrow (t, et)
  | Rec (f, t, body) ->
     let et = infer' (extend ctx (f, t)) body in
     if typ_unify_eq et t then
       t
     else
       type_fail "recursive function does not have the ascribed type"
  | Let (ds, exp) ->
    infer' (infer'_bind ctx ds) exp
  | Apply (e1, e2) ->
     begin match infer' ctx e1 with
     | TArrow (domain, range) ->
        if typ_unify_eq (infer' ctx e2) domain then
          range
        else
          type_fail (Print.exp_to_string e2 ^ " is expected to have a particular type")
     | TVar _ as t ->
        let a = fresh_tvar ()
        and b = fresh_tvar () in
        if typ_unify_eq (TArrow (a, b)) t then
          if typ_unify_eq (infer' ctx e2) a then
            b
          else
            type_fail (Print.typ_to_string t ^ " is not a function type")
        else
          type_fail (Print.exp_to_string e2 ^ " is expected to have a particular type")
     | _ -> type_fail ("Application expected arrow type for " ^ Print.exp_to_string e1)
     end
  (* this line is very strange. if we rename the function to [lookup] then learnocaml will fail *)
  | Var x -> ctx_lookup ctx x
  | Anno (e, t) ->
     if typ_unify_eq (infer' ctx e) t then
       t
     else
       type_fail (Print.exp_to_string e ^ " does not have type a particular type")

and infer'_bind (ctx : context) (ds : dec list) =
  match ds with
  | [] -> ctx
  | (Val (ex, x) | ByName (ex, x)) :: ds ->
     infer'_bind (extend ctx (x, infer' ctx ex)) ds
  | Valtuple (exs, xs) :: ds ->
     begin match infer' ctx exs with
     | TProduct ts ->
        if List.length ts <> List.length xs then
          type_fail "tuple unpack should have the same length as the type on the right hand side";
        infer'_bind (extend_list ctx (List.combine xs ts)) ds
     | TVar _ as t ->
        let vs = Array.to_list (Array.init (List.length xs) (fun _ -> fresh_tvar ())) in
        unify t (TProduct vs);
        infer'_bind (extend_list ctx (List.combine xs vs)) ds
     | _ -> type_fail (Print.exp_to_string exs ^ " is expected to have a product type")
     end

let infer = infer'
