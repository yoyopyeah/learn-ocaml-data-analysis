exception NotImplemented

(* Types in MiniCAML *)
type tp =
  | Arrow of tp list * tp   (* function type: S1 S2 ... Sn -> T *)
  | Int
  | Bool

(* Used for variables, aka "identifiers" *)
type name = string

(* The primitive operations available in MiniCAML *)
type primop = Equals | LessThan | Plus | Minus | Times | Negate

(* Expressions in MiniCAML *)
type exp =
  | I of int                          (* 0 | 1 | 2 | ... *)
  | B of bool                         (* true | false *)
  | If of exp * exp * exp             (* if e then e1 else e2 *)
  | Primop of primop * exp list       (* e1 <op> e2  or <op> e *)
  | Fn of ((name * tp) list * exp)    (* fn (x_1: t_1, ..., x_n: t_n) => e *)
  | Rec of (name * tp * exp)          (* rec (f: t) => e *)
  | Let of (name * exp * exp)         (* let x = e1 in e2 end *)
  | Apply of exp * (exp list)         (* e (e_1, e_2, ..., e_n) *)
  | Var of name                       (* x *)

(* Some example programs in MiniCAML. *)
(* fun (x: int, y: int) => (x * x) + (y * y) *)
let ex1 =
  Fn ([("x", Int); ("y", Int)],
      Primop (Plus,
              [Primop (Times, [Var "x"; Var "x"]);
               Primop (Times, [Var "y"; Var "y"])]))
(* fun () => true *)
let ex2 = Fn ([], B true)
(* let f = (fun (x: int, y: int) => (x * x) + (y * y))
   in
   f (3, 4)
*)
let ex3 =
  Let ("f", ex1,
       Apply (Var "f", [I 3; I 4]))
(* let g = (fun () => true)
   in
   g () *)
let ex4 =
  Let ("g", ex2,
       Apply (Var "g", []))
(* let f = (fun (x: int, y: int) => (x * x) + (y * y))
   in
   f (3)
   Note: this expression is syntactically valid, but ill-typed!
*)
let ex5 =
  Let ("f", ex1,
       Apply (Var "f", [I 3]))
(* let f = (fun (x: int) => (fun (y: int) => (x * x) + (y * y)))
   in
   (f (3)) (4)
*)
let ex6 =
  Let ("f",
       Fn ([("x", Int)],
           Fn ([("y", Int)],
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "x"]);
                        Primop (Times, [Var "y"; Var "y"])]))),
       Apply (Apply (Var "f", [I 3]),
              [I 4]))
(* let f = (fun (x: int) => (fun (y: int) => (x * x) + (y * y)))
   in
   f (3, 4)
   Note: this expression is syntactically valid, but ill-typed!
*)
let ex7 =
  Let ("f",
       Fn ([("x", Int)],
           Fn ([("y", Int)],
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "x"]);
                        Primop (Times, [Var "y"; Var "y"])]))),
       Apply (Var "f", [I 3; I 4]))


(* PART 1: unused_vars *)

(* Deletes every occurence of the elements of xs from l.
   e.g. delete [w; y] [y; x; y; z; w] = [x; z]
*)
let rec delete xs l =
  List.filter (fun x -> not (List.mem x xs)) l

(* free_variables e = list of names occurring free in e
   Invariant: every name occurs at most once.

   The algorithm works from the leaves of the expression tree
   upwards. Every time a variable is encountered, it is considered free.
   When a binding construct is encountered (e.g. Let) the declared
   variable is deleted from the set of free variables formed by the union
   of the recursive calls.
   Other constructs simply form the union of the sets of free variables
   from the recursive calls and return it.
 *)
let rec free_variables =
  (* Taking unions of lists.
     If the lists are in fact sets (all elements are unique),
     then the result will also be a set.
  *)
  let union l1 l2 =
    let l1' = List.filter (fun x -> not (List.mem x l2)) l1 in
    l1' @ l2
  in
  let union_fvs es =
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) -> union_fvs [e; e1; e2]
  | Primop (po, args) -> union_fvs args
  | Fn (xs, e) ->
      let xs = List.map fst xs in
      delete xs (free_variables e)
  | Rec (x, t, e) ->
      delete [x] (free_variables e)
  | Let (x, e1, e2) ->
      let e1_vars = free_variables e1 in
      let e2_vars = delete [x] (free_variables e2) in
      union e1_vars e2_vars
  | Apply (e, es) -> union_fvs (e :: es)
;;

(* PART 2: subst *)

(* A substitution [e/x]. This is read as "e for x". *)
type subst = exp * name
;;

(* PART 3: eval *)

(* Runtime errors that may be raised by eval. *)
type runtime_error =
  | Free_variable of name
  | Bad_primop_args
  | If_non_true_false
  | Arity_mismatch
  | Apply_non_fn

exception Stuck of runtime_error

(* Evaluates a primitive operation *)
let eval_op op exps =
  match op, exps with
  | (Equals,   [I i; I i']) -> Some (B (i = i'))
  | (LessThan, [I i; I i']) -> Some (B (i < i'))
  | (Plus,     [I i; I i']) -> Some (I (i + i'))
  | (Minus,    [I i; I i']) -> Some (I (i - i'))
  | (Times,    [I i; I i']) -> Some (I (i * i'))
  | (Negate,   [I i])       -> Some (I (-i))
  | _                       -> None
;;

(* PART 4: infer *)

(* Type contexts *)
type context = (name * tp) list
let empty = []

(* Looks up the topmost x in ctx and returns its corresponding type.
   If the variable x cannot be located in ctx, raises Not_found.
*)
let lookup (x: name) (ctx: context) = List.assoc x ctx

(* Adds a new type ascription to a context. *)
let extend ctx (x, tau) = (x, tau) :: ctx

(* Adds multiple new type ascriptions to a context. *)
let extend_list (ctx: context) (l: (name * tp) list) =
  List.fold_left extend ctx l

(* Type errors that may be raised by infer *)
type type_error =
  | Free_variable of name
  | Apply_non_arrow of tp (* expected an arrow type, but instead found... *)
  | Arity_mismatch
  | Type_mismatch of tp * tp (* (expected type, actual type) *)

exception TypeError of type_error

(* Convenience function for raising type mismatch errors *)
let type_mismatch expected_type inferred_type =
  raise (TypeError (Type_mismatch (expected_type, inferred_type)))

(* Computes the type of a primitive operation.
   The result is a tuple representing the domain and range of the primop.
 *)
let primopType (p: primop): tp list * tp = match p with
  | Equals   -> ([Int; Int], Bool)
  | LessThan -> ([Int; Int], Bool)
  | Plus     -> ([Int; Int], Int)
  | Minus    -> ([Int; Int], Int)
  | Times    -> ([Int; Int], Int)
  | Negate   -> ([Int], Int)
;;

(* Part 5: Unification *)

(* We extend types to support variables. We also
   simplify arrow types to be of the form S -> T
   instead of S1 ... Sn -> T *)
type utp =
  | UArrow of utp * utp
  | UInt
  | UBool
  | UTVar of (utp option) ref

(* Different errors that can arise during unification. *)
type unif_error =
  (* Raised when attempting to unify a type variable 'a with a type t
     of which 'a is a subexpression, e.g. t is Product ['a; 'a] *)
  | UnifOccursCheckFails
  (* Raised when the unifier attempts to unify mismatched types,
     e.g. Bool with Int, or an Arrow with a Product. *)
  | UnifMismatch of utp * utp
  (* Raised when trying to unify product types with mismatched lengths *)
  | UnifProductMismatch

(* An exception constructor so that we can raise unif_error values. *)
exception UnifError of unif_error

(* Convenience function for raising unif_error values. *)
let unif_error e = raise (UnifError e)

(* `occurs a t` checks whether the type variable `a` appears in the
    type `t`.
*)
let rec occurs a t = match t with
  | UInt | UBool -> false
  | UArrow (t1, t2) -> occurs a t1 || occurs a t2
  | UTVar b ->
    match !b with
    | Some t' -> occurs a t'
    | None -> a == b

(* -------------------------------------------------------------*)
(* Other helper functions                                       *)
(* You don't need to look at these to do the assignment, but it *)
(* would be a good idea to understand them.                     *)
(* -------------------------------------------------------------*)

(* Generating fresh (new) variable names *)
type gen_var = {
  fresh: name -> name; (* generates a fresh name based on a given one. *)
  reset : unit -> unit (* resets the internal counter for making names. *)
}

let gen_var : gen_var =
  let counter = ref 0 in
  let fresh x = incr counter; x ^ (string_of_int (!counter)) in
  let reset () = counter := 0 in
  {fresh; reset}

let freshVar = gen_var.fresh
let resetCtr = gen_var.reset

(* Converts a type to a string representation. *)
let rec string_of_tp t = match t with
  | Arrow (t1s, t2) ->
      (String.concat " " (List.map string_of_tp t1s)) ^
      " -> " ^
      string_of_tp t2
  | Int -> "int"
  | Bool -> "bool"

(* String representations of expressions. Useful for debugging!
   Note that this expression printer is very primitive, but it should suit
   your needs most of the time.
*)
let nl_sep l = String.concat "\n" l

let bracket str = "(" ^ str ^ ")"

let string_of_op p = match p with
  | Equals   -> " = "
  | LessThan -> " < "
  | Plus     -> " + "
  | Minus    -> " - "
  | Times    -> " * "
  | Negate   -> "-"

let rec string_of_exp indent exp =
  let new_ind = indent ^ "  " in
  let string_of_exp' = string_of_exp indent in
  let string_of_exp'' = string_of_exp new_ind in
  match exp with
  | I n ->
      if n < 0 then bracket (string_of_int n)
      else string_of_int n
  | B b -> if b then "True" else "False"
  | If (p, e1, e2) ->
      nl_sep
        ["if " ^ (string_of_exp'' p) ^ " then";
         new_ind ^ (string_of_exp'' e1);
         indent ^ "else";
         new_ind ^ (string_of_exp'' e2)]
  | Primop (p, el) ->
      bracket @@
      if p = Negate then
        (string_of_op p) ^ (string_of_exp' (List.nth el 0))
      else
        (string_of_exp' (List.nth el 0)) ^
        (string_of_op p) ^
        (string_of_exp' (List.nth el 1))
  | Fn (xs, exp) ->
      let params =
        String.concat ", "
          (List.map (fun (x, tp) -> x ^ ": " ^ (string_of_tp tp)) xs)
      in
      bracket @@
      nl_sep
        ["fun (" ^ params ^ ") =>";
         new_ind ^ (string_of_exp'' exp)]
  | Rec (name, tp, exp) ->
      bracket @@
      nl_sep
        ["rec (" ^ name ^ ": " ^ (string_of_tp tp) ^ ") =>";
         new_ind ^ (string_of_exp'' exp)]
  | Let (name, e1, e2) ->
      nl_sep
        ["let " ^ name ^ " = " ^ (string_of_exp' e1) ^ " in";
         new_ind ^ (string_of_exp'' e2)]
  | Apply (e, es) ->
      let params = bracket (String.concat ", " (List.map string_of_exp' es)) in
      (string_of_exp' e) ^ " " ^ params
  | Var name -> name

let print_exp exp = print_string (string_of_exp "" exp)
