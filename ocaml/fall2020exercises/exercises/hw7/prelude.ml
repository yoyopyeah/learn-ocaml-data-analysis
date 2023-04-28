exception NotImplemented

(* Variables *)
type name = string

(* Primitive operations *)
type primop =
  | Equals        (* v1 = v2 *)
  | NotEquals     (* v1 != v2 *)
  | LessThan      (* i1 < i2 *)
  | LessEqual     (* i1 <= i2 *)
  | GreaterThan   (* i1 > i2 *)
  | GreaterEqual  (* i1 >= i2 *)
  | And           (* b1 && b2 *)
  | Or            (* b1 || b2 *)
  | Plus          (* i1 + i2 *)
  | Minus         (* i1 - i2 *)
  | Times         (* i1 * i2 *)
  | Div           (* i1 / i2 *)
  | Negate        (* ~ i *)

(* type exception *)
exception TypeError of string

let type_fail message = raise (TypeError message)

type typ =
  | TArrow   of typ * typ         (* a -> b *)
  | TProduct of typ list          (* a * b *)
  | TInt                          (* int *)
  | TBool                         (* bool *)
  | TVar     of (typ option) ref  (* Only used for Q6 and Q7 *)

let fresh_tvar () = TVar (ref None)

(* type equality ignoring TVar *)
let rec typ_eq t1 e2 =
  match (t1, e2) with
  | (TArrow (domain1, range1), TArrow (domain2, range2)) ->
     typ_eq domain1 domain2 && typ_eq range1 range2
  | (TProduct ts1, TProduct ts2) ->
     List.length ts1 = List.length ts2 && List.for_all2 typ_eq ts1 ts2
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | _ -> false

(* general exception *)
exception Stuck of string

let stuck message = raise (Stuck message)

type exp =
  | Int    of int                        (* 0 | 1 | 2 | ... *)
  | Bool   of bool                       (* true | false *)
  | If     of exp * exp * exp            (* if e then e1 else e2 *)
  | Primop of primop * exp list          (* e1 <op> e2  or  <op> e *)
  | Tuple  of exp list                   (* (e1, ..., eN) *)
  | Fn     of (name * typ option * exp)  (* fn x => e *)
  | Rec    of name * typ * exp           (* rec f => e *)
  | Let    of dec list * exp             (* let decs in e end *)
  | Apply  of exp * exp                  (* e1 e2 *)
  | Var    of name                       (* x *)
  | Anno   of exp * typ                  (* e : t *)

and dec =
  | Val      of exp * name               (* val x = e *)
  | Valtuple of exp * name list          (* val (x1,...,xN) = e *)
  | ByName   of exp * name               (* name x = e1 *)

let eval_op op args =
  match (op, args) with
  | (Equals,       [Int i1; Int i2])   -> Some (Bool (i1 = i2))
  | (NotEquals,    [Int i1; Int i2])   -> Some (Bool (i1 <> i2))
  | (LessThan,     [Int i1; Int i2])   -> Some (Bool (i1 < i2))
  | (LessEqual,    [Int i1; Int i2])   -> Some (Bool (i1 <= i2))
  | (GreaterThan,  [Int i1; Int i2])   -> Some (Bool (i1 > i2))
  | (GreaterEqual, [Int i1; Int i2])   -> Some (Bool (i1 >= i2))
  | (Plus,         [Int i1; Int i2])   -> Some (Int (i1 + i2))
  | (Minus,        [Int i1; Int i2])   -> Some (Int (i1 - i2))
  | (Times,        [Int i1; Int i2])   -> Some (Int (i1 * i2))
  | (Div,          [Int i1; Int i2])   -> Some (Int (i1 / i2))
  | (Negate,       [Int i])            -> Some (Int (-i))
  | _                                  -> None

type context = Ctx of (name * typ) list

(* Context manipulation helpers *)
exception NotFound

let ctx_lookup ctx x =
  let rec assoc x y =
    match y with
    | [] -> raise NotFound
    | (y, r) :: rest ->
       if x = y then
         r
       else
         assoc x rest
  in
  let Ctx list = ctx in assoc x list

let extend ctx (x, v) = let Ctx list = ctx in Ctx ((x,v)::list)

let rec extend_list ctx l =
  match l with
  | [] -> ctx
  | (x, y) :: pairs -> extend_list (extend ctx (x, y)) pairs

(* Replacement for the standard "result" type *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

(* Set helper functions. You might find them useful *)
let member = List.mem

let rec union xs ys =
  match xs with
  | [] -> ys
  | x :: xs ->
     if member x ys then
       union xs ys
     else
       x :: union xs ys

let union_list sets = List.fold_right union sets []

let rec delete ds set =
  match set with
  | [] -> []
  | h :: t ->
     if member h ds then
       delete ds t
     else
       h :: delete ds t


(* free name generator *)
let (fresh_var, reset_ctr) =
  let counter = ref 0 in
  ((fun x ->
    counter := !counter+1;
    string_of_int (!counter) ^ x),
   fun () ->
   counter := 0)

(* Update this to 1 or higher to get debug messages *)
let debug = ref 0

(* example valid MiniML programs *)

let valid_program_1 = "
let fun apply (f : int -> int) : int -> int =
          fn x : int => f(x)
in
  apply (fn x => x * 3) 100
end;
"

let valid_program_2 = "10 * 10 + 33;"

let valid_program_3 = "
let fun fact (x : int) : int =
  if x = 0 then 1
  else x * fact(x - 1)
in
  fact 5
end;
"

let valid_program_4 = "(if true then 3 else 5) : int;"

let valid_program_5 = "
let val x = 1
in
  x + 5
end;
"

let valid_program_6 = "
let val x = true
in
  let val x = 1
  in
    x + 5
  end
end;
"

let valid_program_7 = "
let name x = 3
in
  x + 1
end;
"

let valid_program_8 = "
let val (x,y) = (2 + 1, 2 * 50) in x * x * y end;
"

let valid_program_9 = "
let fun repeat (n : int) : (int -> int) -> int -> int =
          fn f : (int -> int) => fn x : int =>
            if n = 0 then x
            else repeat (n - 1) f (f(x))
in
 repeat 4 (fn z : int => z * 2) 100
 (* expected result: 100 * 2 * 2 * 2 * 2 = 1600 *)
end;
"

let valid_program_10 = "
let val f = let val ten = 10 in (fn y => ten) : int -> int end
in
  f 55
end;
"
