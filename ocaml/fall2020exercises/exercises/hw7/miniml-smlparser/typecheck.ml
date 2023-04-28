open Type
open Minml
  
type context = Ctx of (name * typ) list

let empty = Ctx []

exception NotFound

let ctx_lookup ctx x =
  let rec assoc x y = match y with
    | [] -> raise NotFound
    | (y, r)::rest -> if x = y then r else assoc x rest
  in
  let Ctx list = ctx in assoc x list

let extend ctx (x, v) = let Ctx list = ctx in Ctx ((x,v)::list)

let rec extend_list ctx l = match l with
  | [] -> ctx
  | (x,y) :: pairs -> extend_list (extend ctx (x, y)) pairs

exception TypeError of string

let type_fail message = raise (TypeError message)

let rec infer ctx e : typ =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | If (cond, e1, e2) ->
     if not (Type.eq (infer ctx cond) TBool) then
       type_fail "condition of if should have type bool";
     let t1 = infer ctx e1
     and t2 = infer ctx e2 in
     if not (Type.eq t1 t2) then
       type_fail "branches of if should have the same type";
     t1
  | Primop (op, es) ->
     begin match op with
     | LessThan | GreaterThan | LessEqual | GreaterEqual
       | Plus | Minus | Times | Div ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer ctx e1
           and t2 = infer ctx e2 in
           if Type.eq t1 TInt && Type.eq t2 TInt then TInt
           else type_fail "operator should take two integers"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | And | Or ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer ctx e1
           and t2 = infer ctx e2 in
           if Type.eq t1 TBool && Type.eq t2 TBool then TBool
           else type_fail "logic operator should take two boolean"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | Equals | NotEquals ->
        begin match es with
        | [e1; e2] ->
           let t1 = infer ctx e1
           and t2 = infer ctx e2 in
           if Type.eq t1 t2 then TBool
           else type_fail "equal comparison should take two operands of the same type"
        | _ -> type_fail "binary operator. shouldn't hit this case due to the parser"
        end
     | Negate ->
        begin match es with
        | [e] ->
           let t = infer ctx e in
           if Type.eq t TInt then TInt
           else type_fail "operator should take an integer"
        | _ -> type_fail "negate. shouldn't hit this case due to the parser"
        end
     end
  | Tuple es ->
     TProduct (List.map (infer ctx) es)
  | Fn (_, None, e) ->
     type_fail ("Can't infer type of expression " ^ Print.expToString e)
  | Fn (x, Some t, e) ->
     let et = infer (extend ctx (x, t)) e in
     TArrow (t, et)
  | Rec (f, ftype, body) ->
     let et = infer (extend ctx (f, ftype)) body in
     if Type.eq et ftype then ftype
     else type_fail (Format.sprintf "recursive function %s does not have the ascribed type" (Print.expToString e))
  | Let (decs, exp) ->
    infer (infer_bind ctx decs) exp
  | Apply (e1, e2) ->
     begin match infer ctx e1 with
     | TArrow (domain, range) ->
        if Type.eq (infer ctx e2) domain then range
        else type_fail (Format.sprintf "expression %s is expected to have type %s"
                                  (Print.expToString e2) (Type.toString domain))
     | _ -> type_fail ("Application expected arrow type for " ^ Print.expToString e1)
     end
  | Var x -> ctx_lookup ctx x
  | Anno (e, t) ->
     if Type.eq (infer ctx e) t then t
     else type_fail (Format.sprintf "expression %s does not have type %s"
                               (Print.expToString e) (Type.toString t))

and infer_bind ctx decs =
  match decs with
  | [] -> ctx
  | (Val (e, x) | ByName (e, x)) :: ds ->
     infer_bind (extend ctx (x, infer ctx e)) ds
  | Valtuple (e, xs) :: ds ->
     begin match infer ctx e with
     | TProduct ts ->
        if List.length ts <> List.length xs then
          type_fail "tuple unpack should have the same length as the type on the right hand side";
        infer_bind (extend_list ctx (List.combine xs ts)) ds
     | t -> type_fail (Format.sprintf "%s is expected to have a product type, but it has type %s"
                                 (Print.expToString e) (Type.toString t))
     end

let rec find_root t =
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

let rec self_referential r t =
  let t = find_root t in
  match t with
  | TInt | TBool -> false
  | TVar r' -> r == r'
  | TProduct ts -> List.exists (self_referential r) ts
  | TArrow (d, c) -> self_referential r d || self_referential r c

let rec unify t1 t2 : unit =
  let t1 = find_root t1
  and t2 = find_root t2 in
  match t1, t2 with
  | TInt, TInt
    | TBool, TBool -> ()
  | TProduct ts1, TProduct ts2 ->
     if List.length ts1 <> List.length ts2
     then type_fail (Format.sprintf "%s and %s cannot be unified because they have different length"
                                    (Type.toString t1) (Type.toString t2));
     List.iter2 unify ts1 ts2
  | TArrow (dom1, cod1), TArrow (dom2, cod2) ->
     unify dom1 dom2;
     unify cod1 cod2
  | TVar r1, TVar r2 when r1 == r2 -> ()
  | TVar r1, _ ->
     begin match !r1 with
     | None ->
        if self_referential r1 t2 then
          type_fail "a type variable is unified to a type referring to itself";
        r1 := Some t2
     | Some t1 -> unify t1 t2
     end
  | _, TVar r2 ->
     begin match !r2 with
     | None ->
        if self_referential r2 t1 then
          type_fail "a type variable is unified to a type referring to itself";
        r2 := Some t1
     | Some t2 -> unify t1 t2
     end
  | _, _ -> type_fail (Format.sprintf "%s and %s cannot be unified" (Type.toString t1) (Type.toString t2))
