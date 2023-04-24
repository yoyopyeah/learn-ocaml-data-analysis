(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  raise NotImplemented

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  raise NotImplemented

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  raise NotImplemented

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)

(* ---------- Hamming Numbers ----------- *)

let rec merge s1 s2 = 
  ()

let rec hamming_series = 
  ()