open Lib

module M = Minml

let paren lvl oplvl string =
  if oplvl < lvl then "(" ^ string ^ ")"
  else string

let po_prec p =
  match p with
  | M.Or           -> 1
  | M.And          -> 2
  | M.Equals       -> 3
  | M.NotEquals    -> 3
  | M.LessThan     -> 3
  | M.GreaterThan  -> 3
  | M.LessEqual    -> 3
  | M.GreaterEqual -> 3
  | M.Plus         -> 4
  | M.Minus        -> 4
  | M.Times        -> 5
  | M.Div          -> 5
  | M.Negate       -> 7

let rec po p =
  match p with
  | M.Or           -> "||"
  | M.And          -> "&&"
  | M.Equals       -> "="
  | M.NotEquals    -> "!="
  | M.LessThan     -> "<"
  | M.GreaterThan  -> ">"
  | M.LessEqual    -> "<="
  | M.GreaterEqual -> ">="
  | M.Plus         -> "+"
  | M.Minus        -> "-"
  | M.Times        -> "*"
  | M.Div          -> "/"
  | M.Negate       -> "~"

and expstr lvl e =
  let f = expstr in
  match e with
  | M.Int i -> string_of_int i
  | M.Bool true -> "true"
  | M.Bool false -> "false"
  | M.If (ec, et, ef) ->
     paren lvl 1 ("if " ^ f 2 ec ^ " then " ^ f 2 et ^ " else " ^ f 2 ef)
  | M.Primop (p, []) -> "(bad primop)"
  | M.Primop (p, [e]) -> paren lvl 7 (po p ^ f 7 e)
  | M.Primop (p, e::es) ->
     let f' b a = b ^ " " ^ po p ^ " " ^ (f (po_prec p) a) in
     paren lvl (po_prec p) (List.fold_left f' (f (po_prec p) e) es)
  | M.Tuple es -> "(" ^ separate ", " (f 0) es ^ ")"
  | M.Fn (x, t, e) ->
     begin match t with
     | None -> paren lvl 2 ("fn " ^ x ^  " => " ^ f 0 e)
     | Some t -> paren lvl 2 ("fn " ^ x ^ ": " ^ Type.toString t ^ " => " ^ f 0 e)
     end
  | M.Rec (ff, ftype, e) ->
     paren lvl 2 ("rec " ^ ff ^  " : " ^ Type.toString ftype ^ " => " ^ f 0 e)
  | M.Let (decs, e) ->
     "let " ^ separate "\n    " decToString decs ^ " in " ^ f 0 e ^ " end"
  | M.Apply (e1, e2) ->
     paren lvl 6 ((f 6 e1) ^ " " ^ (f 7 e2))
  | M.Var v -> v
  | M.Anno (e, t) -> paren lvl 0 (f 1 e ^ " : " ^ Type.toString t)

and expToString e = expstr 0 e

and decToString e =
  let f = expToString in
  match e with
  | M.Val (M.Rec (ff, ftype, M.Fn(x, _, body)) as r, gg) ->
     if ff = gg then
       "fun " ^ ff ^ " : " ^ Type.toString ftype ^ " " ^ x ^  " = " ^ f body
     else "val " ^ gg ^ " = " ^ f r

  | M.Val (e1, x) ->
     "val " ^ x ^ " = " ^ f e1

  | M.ByName (e1, x) ->
     "name " ^ x ^ " = " ^ f e1

  | M.Valtuple (e1, xs) ->
     "val (" ^ separate ", " (fun name -> name) xs ^ ") = " ^ f e1

let rec astToString e = match e with
  | M.Int i -> "Int (" ^ string_of_int i ^ ")"
  | M.Bool true -> "Bool true"
  | M.Bool false -> "Bool false"
  | M.If (ec, et, ef) ->
     "If (" ^ astToString ec ^  ", "
     ^ astToString et ^ ", " ^ astToString ef ^ ")"
  | M.Primop (p, []) -> "(bad primop)"
  | M.Primop (p, [e]) -> "Primop (" ^ (po p ^ astToString e) ^ ")"
  | M.Primop (p, es) -> "Primop (" ^ po p ^ "," ^
                          (List.fold_left (fun s e -> astToString e ^ ", " ^ s) "" es) ^ ")"
  | M.Tuple es -> "Tuple (" ^ separate ", " astToString es ^ ")"
  | M.Fn (x, t, e) ->
     begin match t with
     | None -> "Fn (" ^ x ^  ",  None, " ^ astToString e ^ ")"
     | Some t -> "Fn (" ^ x ^  ",  Some (" ^ astTypeToString t ^ "), " ^ astToString e ^ ")"
     end
  | M.Rec (ff, ftype, e) ->
     "Rec (" ^ ff ^  ", " ^ astTypeToString ftype ^ ", " ^ astToString e ^ ")"
  | M.Let (decs, e) ->
     "Let ([" ^ separate ", " astDecToString decs ^ "], " ^ astToString e ^ ")"
  | M.Apply (e1, e2) ->
     "Apply (" ^ astToString e1 ^ ", " ^ astToString e2 ^ ")"
  | M.Var v -> "Var " ^ v
  | M.Anno (e, t) -> "Anno (" ^ astToString e ^ ", " ^ astTypeToString t ^ ")"

and astDecToString e = match e with
  | M.Val (e1, x) ->
     "Val (" ^ astToString e1 ^ ", " ^ x ^ ")"

  | M.ByName (e1, x) ->
     "ByName (" ^ astToString e1 ^ ", " ^ x ^ ")"

  | M.Valtuple (e1, xs) ->
     "ValTuple (" ^ astToString e1 ^ ", [" ^ separate ", " (fun name -> name) xs ^ "])"

and astTypeToString e = match e with
  | Type.TArrow (t1, t2) ->
     "TArrow (" ^ astTypeToString t1 ^ ", " ^ astTypeToString t2 ^ ")"
  | Type.TProduct [] -> "(EmptyProduct)"
  | Type.TProduct xs -> "Product [" ^ separate ", " astTypeToString xs ^ "]"
  | Type.TInt -> "Int"
  | Type.TBool -> "Bool"
  | Type.TVar x ->
     "TVar (" ^ (match !x with
                 | None -> "None"
                 | Some t -> "Some" ^ astTypeToString t) ^ ")"
