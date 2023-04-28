module Exp = struct
  type primop = Equals | LessThan | Plus | Minus | Times | Negate

  type exp =
    | Int of int                      (* 0 | 1 | 2 | ... *)
    | Bool of bool                    (* true | false *)
    | If of exp * exp * exp           (* if e then e1 else e2 *)
    | Primop of primop * exp list     (* e1 <op> e2  or  <op> e *)
end        

module Eval = struct 
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
end




            
            
module Types = struct
  module E = Exp
           
  type tp = Int | Bool
                
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
                  
                  
  let rec infer e = match e with 
    | E.Int _ -> Int
    | E.Bool _ -> Bool
    | E.If (e, e1, e2) -> 
       (match infer e with 
        | Bool -> let t1 = infer e1 in 
                  let t2 = infer e2 in 
                  if t1 = t2 then t1 
                  else fail ("Expected " ^ typ_to_string t1 ^ 
                               " - Inferred " ^ typ_to_string t2)
        | t -> fail ("Expected Bool\nInferred " ^ typ_to_string t))
    | E.Primop (po, args) -> 
       let (expected_arg_types, resultType) = primopType po in 
       let inferred_arg_types = List.map infer args in 
       
       let rec compare tlist1 tlist2 = match tlist1, tlist2 with 
         | [] , [] -> resultType 
         | t::tlist , s::slist -> 
            if t = s then compare tlist slist
            else fail ("Expected " ^ typ_to_string t ^ 
                         " - Inferred " ^ typ_to_string s)
         | _ , _ -> fail ("Error: Primitve operator used with incorrect number of arguments")
       in 
       compare expected_arg_types inferred_arg_types 
       
end 


module E = Exp
let e1 = E.If (E.Primop (E.Equals, [E.Int 3; E.Int 2]), 
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))

let e2 = E.If (E.Primop (E.Equals, [E.Int 3; E.Bool true]), 
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))
