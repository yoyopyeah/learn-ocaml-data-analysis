let rec parseExp toklist sc =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp toklist sc =
  parsePExp
    toklist
    (fun tlist1 exp1 -> match tlist1 with
       | PLUS :: toklist' ->
           parseSExp
             toklist'
             (fun tlist2 exp2 -> sc tlist2 (Sum (exp1, exp2)))
       | SUB :: toklist' ->
           parseSExp
             toklist'
             (fun tlist2 exp2 -> sc tlist2 (Minus (exp1, exp2)))
       | _ -> sc tlist1 exp1)

and parsePExp toklist sc =
  parseAtom
    toklist
    (fun tlist1 exp1 -> match tlist1 with
       | TIMES :: toklist' ->
           parsePExp
             toklist'
             (fun tlist2 exp2 -> sc tlist2 (Prod (exp1, exp2)))
       | DIV :: toklist' ->
           parsePExp
             toklist'
             (fun tlist2 exp2 -> sc tlist2 (Div (exp1, exp2)))
       | _ -> sc tlist1 exp1)

and parseAtom toklist sc = match toklist with
  | INT n :: toklist -> sc toklist (Int n)
  | LPAREN :: toklist ->
      parseSExp
        toklist
        (fun toklist' exp -> match toklist' with
           | RPAREN :: toklist'' -> sc toklist'' exp
           | _ -> raise (Error "Expected right parenthesis"))
  | _ -> raise (Error "Expected Atomic Expression")

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* Evaluator *)
(* eval : string -> int *)
let eval e = eval' (parse e)



(* Q2 *)

let rec merge s1 s2 = 
    { hd = if s1.hd < s2.hd then s1.hd 
      else  s2.hd ; 
      tl = 
  Susp (fun () -> 
    if s1.hd = s2.hd then 
      merge (force s1.tl) (force s2.tl)
    else (if s1.hd < s2.hd then merge (force s1.tl) s2
      else merge s1 (force s2.tl)))
    }


(* Hamming series *) 
let rec hamming_series = 
  let times k = map (fun x -> x * k) in
  {hd = 1 ; 
   tl = Susp (fun () -> merge (times 2 hamming_series)
            (merge (times 3 hamming_series)
             (times 5 hamming_series)))
  }
