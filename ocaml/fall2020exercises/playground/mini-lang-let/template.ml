module Exp = 
struct
  type name   = string
  type primop = Equals | LessThan | Plus | Minus | Times | Negate

  type exp =
    | Var of name
    | Int of int                      (* 0 | 1 | 2 | ... *)
    | Bool of bool                    (* true | false *)
    | If of exp * exp * exp           (* if e then e1 else e2 *)
    | Primop of primop * exp list     (* e1 <op> e2  or  <op> e *)
    | Let of dec * exp                (* let dec in e end *)
    | App of exp * exp                (* e1 e2 *)
    | Fn  of name * exp               (* fn x => e *)
    | Rec of name * exp               (* rec f => e *)

  and dec = 
    | Val of exp * name               (* val x = e *)


  (* ---------------------------------------------------------------- *)
  (* Generating new variable names *)

  let genCounter = 
  let counter = ref 0 in
  ((fun x -> 
    let _ = counter := !counter+1 in
    x ^ string_of_int (!counter)),
  fun () ->
    counter := 0)

  let (freshVar, resetCtr) = genCounter

 (* ---------------------------------------------------------------- *)
 (* Basic functions about lists *)

  let member x l = List.exists (fun y -> y = x) l

  let rec delete (vlist, l) = match l with
    |  [] -> []
    |  h :: t -> 
       if member h vlist then delete (vlist, t)
       else h :: delete (vlist, t)
         
  let rec union p = match p with
  | ([], l) -> l
  | (x::t, l) -> 
    if member x l then
      union (t, l)
    else
      x :: union (t, l)

  (* ---------------------------------------------------------------- *)
  (* Computing the set of free variables in an expression *)
        
  let rec freeVars e = match e with
  | Var y -> [y]
  | Int n -> []
  | Bool b -> []
  | If(e, e1, e2) ->
    union (freeVars e, union (freeVars e1, freeVars e2))
  | Primop (po, args) ->
    List.fold_right (fun e1 fv -> union (freeVars e1, fv)) args []
  | Let (Val (e1, x), e2) ->
      union (freeVars e1, delete ([x], freeVars e2))

  (* ---------------------------------------------------------------- *)
  (* Substitution 
   subst : (exp * name) -> exp -> exp

   subst (e',x) e = [e'/x]e

   subst replaces every occurrence of the variable x
   in the expression e with e'.
  *)

  let rec subst ((e',x) as s) exp =  match exp with
    | Var y ->
       if x = y then e'
       else Var y         
    | Int n  -> Int n
    | Bool b -> Bool b
    | Primop(po, args) -> 
       Primop(po, List.map (subst s) args)
    | If(e, e1, e2) ->
       If(subst s e, subst s e1, subst s e2)
    | Let (Val(e1,y), e2) -> 
       let e1' = subst s e1 in 
       if x = y then 
         (* optimization: don't traverse e2 as there is not free occurrence of x in e2 *)
         Let (Val (e1', y), e2) 
       else 
         if member y (freeVars e') then 
           let y'  = freshVar y in 
           let e2' = rename (Var y', y) e2 in 
           Let(Val(e1', y'), subst s e2')
         else 
           Let(Val(e1', y), subst s e2)

  and rename r e = subst r e
             
       
       
end        

module Eval = 
  struct 
    open Exp

    exception Stuck of string 

    let evalOp op = match op with
      | (Equals,   [Int i; Int i']) -> Some (Bool (i = i'))
      | (LessThan, [Int i; Int i']) -> Some (Bool (i < i'))
      | (Plus,     [Int i; Int i']) -> Some (Int (i + i'))
      | (Minus,    [Int i; Int i']) -> Some (Int (i - i'))
      | (Times,    [Int i; Int i']) -> Some (Int (i * i'))
      | (Negate,   [Int i])         -> Some (Int (-i))
      | _                           -> None

    let rec eval e = match e with 
      | Int _ -> e
      | Bool _ -> e
      | If(e, e1, e2) ->
         (match eval e with
         | Bool true -> eval e1
         | Bool false -> eval e2
         | _ -> raise (Stuck "guard is not a bool"))
      (* primitive operations +, -, *, <, = *)
      | Primop (po, args) ->
         let argvalues = List.map eval args in
         (match evalOp (po, argvalues) with
         | None -> raise (Stuck "Bad arguments to primitive operation")
         | Some v -> v)
      | Let (Val (e1,x), e2) -> 
     eval (subst (eval e1, x) e2)
       
  end

module Types = 
  struct
    module E = Exp

    type tp = Int | Bool

    type ctx = (E.name * tp) list

    let typ_to_string t = match t with 
      | Int -> "Int"
      | Bool -> "Bool"

    exception TypeError of string

    let fail message = raise (TypeError message)
      
    (* primopType p = (argTypes, returnType) *)
    let primopType p = match p with
      | E.Equals   -> ([Int; Int], Bool)
      | E.LessThan -> ([Int; Int], Bool)
      | E.Plus     -> ([Int; Int], Int)
      | E.Minus    -> ([Int; Int], Int)
      | E.Times    -> ([Int; Int], Int)
      | E.Negate   -> ([Int], Int)

    let rec infer gamma e = match e with 
      | E.Var x -> List.assoc x gamma 
      | E.Int _ -> Int
      | E.Bool _ -> Bool
      | E.If (e, e1, e2) -> 
         (match infer gamma e with 
         | Bool -> let t1 = infer gamma e1 in 
                   let t2 = infer gamma e2 in 
                   if t1 = t2 then t1 
                   else fail ("Expected " ^ typ_to_string t1 ^ 
                              " - Inferred " ^ typ_to_string t2)
         | t -> fail ("Expected Bool\nInferred " ^ typ_to_string t))
      | E.Primop (po, args) -> 
         let (expected_arg_types, resultType) = primopType po in 
         let inferred_arg_types = List.map (infer gamma) args in 

         let rec compare tlist1 tlist2 = match tlist1, tlist2 with 
           | [] , [] -> resultType 
           | t::tlist , s::slist -> 
              if t = s then compare tlist slist
              else fail ("Expected " ^ typ_to_string t ^ 
                         " - Inferred " ^ typ_to_string s)
           | _ , _ -> fail ("Error: Primitve operator used with incorrect number of arguments")
         in 
           compare expected_arg_types inferred_arg_types            

      | E.Let (E.Val (e1, x), e2) -> 
         let t = infer gamma e1 in 
           infer ((x,t) :: gamma) e2
  end 


module E = Exp
let e1 = E.If (E.Primop (E.Equals, [E.Int 3; E.Int 2]), 
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))

let e2 = E.If (E.Primop (E.Equals, [E.Int 3; E.Bool true]), 
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))


let e3 = E.Let (E.Val (E.Int 3, "x"), E.Primop (E.Plus, [E.Var "x" ; E.Int 2]))

let e4 = E.Let (E.Val (E.Int 3, "x"), 
                E.Let (E.Val (E.Int 2, "y"), E.Primop (E.Plus, [E.Var "x" ; E.Var "y"])))

let e5 = E.Let (E.Val (E.Bool true, "x"), 
                E.Let (E.Val (E.Int 3, "x"), E.Primop (E.Plus, [E.Var "x" ; E.Var "x"])))

let e6 = E.Let (E.Val (E.Bool true, "x"), 
                E.Let (E.Val (E.Int 3, "x"), E.If (E.Var "x" , E.Int 3, E.Int 2)))

