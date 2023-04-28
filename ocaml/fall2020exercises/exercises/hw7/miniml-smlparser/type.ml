type typ =
| TArrow of typ * typ
| TProduct of typ list
| TInt
| TBool
| TVar of (typ option) ref (* Only used for unification and bonus *)

let rec eq t1 e2 = match t1, e2 with
  | (TArrow(domain1, range1), TArrow(domain2, range2)) ->
    eq domain1 domain2 && eq range1 range2
  | (TProduct ts1, TProduct ts2) ->
    List.length ts1 = List.length ts2 && List.for_all2 eq ts1 ts2
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | (_, _) -> false

let paren lvl oplvl string =
  if oplvl < lvl then "(" ^ string ^ ")" else string

let rec member l r = match l with
  | [] -> None
  | (a, r') :: l' -> if r = r' then Some a else member l' r

let toString typ =
  let counter = ref 0 in
  let freshVar () =
    let _ = counter := !counter + 1 in
    "a" ^ (string_of_int !counter)
  in
  let rec s lvl l typ = match typ with
    | TArrow(domain, range) ->
       let (l', t) = s 1 l domain in
       let (l'', t') = s 0 l' range in
       (l'', paren lvl 0 (t ^ " -> " ^ t'))
    | TProduct [] -> (l, "()")
    | TProduct [x] -> s lvl l x
    | TProduct (t :: ts) ->
       let f = fun (l, t) p ->
         let (l', t') = s 2 l p in
         (l', t ^ " * " ^ t')
       in
       let (l', t') = List.fold_left f (s 2 l t) ts in
       (l', paren lvl 1 t')
    | TInt -> (l, "int")
    | TBool -> (l, "bool")
    | TVar x ->
       match !x with
       | None -> (match member l x with
                  | None -> let a = freshVar () in ((a, x) :: l, a)
                  | Some a -> (l, a))
       | Some t -> s 1 l t
  in
  let (_, t) = s 0 [] typ in
  t
