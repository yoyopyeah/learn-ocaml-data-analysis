open Minml
open Type

exception Error of string

type token =
  | VAR of string
  | VAL
  | TRUE
  | TIMES
  | THEN
  | SEMICOLON
  | RPAREN
  | PLUS
  | NUM of int
  | NEGATE
  | NAME
  | MINUS
  | DIV
  | LPAREN
  | LET
  | INT
  | IN
  | IF
  | FUN
  | FN
  | FALSE
  | EQUAL
  | NEQUAL
  | LT
  | GT
  | LE
  | GE
  | AND
  | OR
  | EOF
  | END
  | ELSE
  | DARROW
  | COMMA
  | COLON
  | BOOL
  | ARROW

let token_to_str tok =
  match tok with
  | VAR x -> Format.sprintf "VAR %s" x
  | VAL -> "VAL"
  | TRUE -> "TRUE"
  | TIMES -> "TIMES"
  | THEN -> "THEN"
  | SEMICOLON -> "SEMICOLON"
  | RPAREN -> "RPAREN"
  | PLUS -> "PLUS"
  | NUM i -> Format.sprintf "NUM %d" i
  | NEGATE -> "NEGATE"
  | NAME -> "NAME"
  | MINUS -> "MINUS"
  | DIV -> "DIV"
  | LPAREN -> "LPAREN"
  | LET -> "LET"
  | INT -> "INT"
  | IN -> "IN"
  | IF -> "IF"
  | FUN -> "FUN"
  | FN -> "FN"
  | FALSE -> "FALSE"
  | EQUAL -> "EQUAL"
  | NEQUAL -> "NEQUAL"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | AND -> "AND"
  | OR -> "OR"
  | EOF -> "EOF"
  | END -> "END"
  | ELSE -> "ELSE"
  | DARROW -> "DARROW"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | BOOL -> "BOOL"
  | ARROW -> "ARROW"

module S = struct

  type 'a t = Stream of (unit -> 'a front)
  and 'a front =
    | Nil
    | Cons of ('a * 'a t)

  let delay f =
    let memo = ref None in
    let memof () =
      match !memo with
      | Some g -> g
      | None   ->
         let r = f () in
         begin
           memo := Some r;
           r
         end
    in Stream memof

  let force (Stream f) = f ()

  let rec iterate f =
    delay (fun () ->
        match f () with
        | None -> Nil
        | Some x -> Cons (x, iterate f))

end

module MSGS = struct

  let rparen_or_comma = "expect a right parenthesis or a comma"
  let variable = "expect a variable"

end

let lexer_to_stream lexer (lexbuf : Lexing.lexbuf) =
  S.iterate (fun () -> Some (lexer lexbuf))

let next_msg s msg =
  match S.force s with
  | S.Nil -> raise (Error msg)
  | S.Cons (h, t) -> (h, t)

(** next s = (x,s'), where x is the head of s, s' the tail of s
    raises Error if stream is empty
 *)
let next s = next_msg s "Unexpected end of stream"

(** match tok s = s', s' is the tail of s
    raises Error if head of s does not match tok
 *)
let tok_match tok s =
  let msg = Format.sprintf "Expected %s token" (token_to_str tok) in
  let (n, s') = next_msg s msg in
  if tok = n then s'
  else raise (Error msg)

let build_primop e' (primop, e) =
  Primop (primop, [e'; e])

let build_primops exp op_exps =
  List.fold_left build_primop exp op_exps

(** parse_program r = (e,s')
    where e is the result of parsing the beginning of r
    and s' the unprocessed tail of r
 *)
let rec parse_program r =
  let (e, s) = parse_exp (S.delay (fun () -> S.Cons r))
  in (e, tok_match SEMICOLON s)

(** parse_factors: Recursively consume adjacent atomic factors (parse_factora),
    forming them into a chain of applications. *)
and parse_factors s eo =
  match parse_factor_option s with
  | Some (e, s) ->
     begin match eo with
     | None -> parse_factors s (Some e)
     | Some f -> parse_factors s (Some (Apply (f, e)))
     end
  | None ->
     match eo with
     | None -> raise (Error "Expected expression")
     | Some e -> (e, s)

and parse_factor es = parse_factors es None

and parse_tuple s =
  let (e, s) = parse_exp s in
  let (n, s') = next_msg s MSGS.rparen_or_comma in
  if n = COMMA then
    let (es, s') = parse_tuple s' in
    (e :: es, s')
  else ([e], s)

(** parse_factora (t,s) attempts to find an atomic expression (no applications)
    starting with the token t, perhaps continuing through the stream.
    Returns SOME(e, s) if the exp e was successfully recognized, with s
    the stream remaining after it.
    Returns NONE if the token cannot begin any exp.
    May raise exception Error if the input stream does not represent
    any valid MinML program. *)
and parse_factora (tok, s) =
  match tok with
  | TRUE -> Some (Bool true, s)
  | FALSE -> Some (Bool false, s)
  | NUM n -> Some (Int n, s)
  | VAR v -> Some (Var v, s)
  | IF ->
     let (ec, s) = parse_exp s in
     let s = tok_match THEN s in
     let (et, s) = parse_exp s in
     let s = tok_match ELSE s in
     let (ef, s) = parse_exp s in
     Some (If (ec, et, ef), s)
  | LPAREN ->
     let (e, s) = parse_exp s in
     let (n, s) = next_msg s MSGS.rparen_or_comma in
     begin match n with
     | RPAREN -> Some (e, s)
     | COMMA ->
        let (es, s) = parse_tuple s in
        Some (Tuple (e :: es), tok_match RPAREN s)
     | _ -> raise (Error MSGS.rparen_or_comma)
     end
  | FN ->
     let (x, s) = parse_var s in
     let (n, s') = next_msg s "expect a colon or a =>" in
     let (t, s) =
       if n = COLON then
         let (t, s) = parse_type s' in
         (Some t, s)
       else (None, s) in
     let s = tok_match DARROW s in
     let (e, s) = parse_exp s in
     Some (Fn (x, t, e), s)
  | LET ->
     let (ds, s) = parse_decs s in
     let s = tok_match IN s in
     let (e, s) = parse_exp s in
     let s = tok_match END s in
     Some (Let (ds, e), s)
  | NEGATE ->
     begin match parse_factora (next_msg s "expect a operand after ~") with
     | None -> None
     | Some (e, s) -> Some (Primop (Negate, [e]), s)
     end
  | _ -> None

and parse_dec t s =
  match t with
  | VAL ->
     let (n, s') = next_msg s "expect a variable or a left parenthesis" in
     if n = LPAREN then
       let (xs, s) = parse_tup_vars s' in
       let s = tok_match RPAREN s in
       let s = tok_match EQUAL s in
       let (e, s) = parse_exp s in
       (Valtuple (e, xs), s)
     else
       let (x, s) = parse_var s in
       let s = tok_match EQUAL s in
       let (e, s) = parse_exp s in
       (Val (e, x), s)
  | NAME ->
     let (x, s) = parse_var s in
     let s = tok_match EQUAL s in
     let (e, s) = parse_exp s in
     (ByName (e, x), s)
  | FUN ->
     let (f, s) = parse_var s in
     let (xs, s) = parse_args s in
     let s = tok_match COLON s in
     let (t, s) = parse_type s in
     let s = tok_match EQUAL s in
     let (e, s) = parse_exp s in
     let g (n, o) ts = match o with
       | Some t -> TArrow (t, ts)
       | None -> TArrow (TVar (ref None), ts)
     in
     let ft = List.fold_right g xs t in
     let e' = List.fold_right (fun (n, t) e -> Fn (n, t, e)) xs e in
     (Val (Rec (f, ft, e'), f), s)
  | _ -> raise (Error "shouldn't occur")

and parse_decs s =
  let (n, s') = next_msg s "expect val, name, fun or in" in
  match n with
  | VAL | NAME | FUN ->
     let (d, s) = parse_dec n s' in
     let (ds, s) = parse_decs s in
     (d :: ds, s)
  | _ -> ([], s)

and parse_args s =
  let (n, s') = next_msg s "expect a variable, left parenthesis or a colon" in
  match n with
  | VAR x ->
     let (xs, s) = parse_args s' in
     (x, None) :: xs, s
  | LPAREN ->
     let (x, s) = parse_var s' in
     let s = tok_match COLON s in
     let (t, s) = parse_type s in
     let s = tok_match RPAREN s in
     let (xs, s) = parse_args s in
     (x, Some t) :: xs, s
  | _ -> [], s

and parse_var s =
  let (n, s) = next_msg s MSGS.variable in
  match n with
  | VAR x -> (x, s)
  | _ -> raise (Error MSGS.variable)

and parse_tup_vars' s =
  let (x, s) = parse_var s in
  let (n, s') = next_msg s MSGS.rparen_or_comma in
  if n = COMMA then
    let (xs, s) = parse_tup_vars' s'
    in (x :: xs, s)
  else ([x], s)

and parse_tup_vars s =
  let (x, s) = parse_var s in
  let s = tok_match COMMA s in
  let (xs, s) = parse_tup_vars' s in
  (x :: xs, s)

and parse_factor_option s =
  match S.force s with
  | S.Nil -> None
  | S.Cons res -> parse_factora res

and parse_or_aux acc s =
  let relop = match next s with
    | (OR, s) -> Some (Or, s)
    | _  -> None in
  match relop with
  | Some (op, s) ->
     let (e, s) = parse_and s
     in parse_or_aux (acc @ [(op, e)]) s
  | None -> (acc, s)

and parse_or es =
  let (e, s) = parse_and es in
  let (exp's, s) = parse_or_aux [] s in
  (build_primops e exp's, s)

and parse_exp s =
  let (e, s) = parse_or s in
  match next s with
  | COLON, s ->
     let (t, s) = parse_type s in
     (Anno (e, t), s)
  | _ -> e, s

and parse_and_aux acc s =
  let relop = match next s with
    | (AND, s) -> Some (And, s)
    | _  -> None in
  match relop with
  | Some (op, s) ->
     let (e, s) = parse_and s
     in parse_and_aux (acc @ [(op, e)]) s
  | None -> (acc, s)

and parse_and s =
  let (e, s) = parse_comp s in
  let (ands, s) = parse_and_aux [] s in
  build_primops e ands, s

and parse_comp_aux acc s =
  let relop = match next s with
    | (EQUAL, s) -> Some (Equals, s)
    | (NEQUAL, s) -> Some (NotEquals, s)
    | (LT, s) -> Some (LessThan, s)
    | (LE, s) -> Some (LessEqual, s)
    | (GT, s) -> Some (GreaterThan, s)
    | (GE, s) -> Some (GreaterEqual, s)
    | _  -> None in
  match relop with
  | Some (op, s) ->
     let (e, s) = parse_comp s
     in parse_comp_aux (acc @ [(op, e)]) s
  | None -> (acc, s)

and parse_comp s =
  let (e, s) = parse_exp' s in
  let (comps, s) = parse_comp_aux [] s in
  build_primops e comps, s

and parse_exp'_aux acc s =
  let addop = match next s with
    | (PLUS, s) -> Some (Plus, s)
    | (MINUS, s) -> Some (Minus, s)
    | _ -> None
  in match addop with
     | Some (addop, s) ->
        let (e, s) = parse_term s in
        parse_exp'_aux (acc @ [(addop, e)]) s
     | None -> (acc, s)

and parse_exp' es =
  let (e, s) = parse_term es in
  let (terms, s) = parse_exp'_aux [] s in
  build_primops e terms, s

and parse_term_aux acc s =
  let mulop = match next s with
    | (TIMES, s) -> Some (Times, s)
    | (DIV, s) -> Some (Div, s)
    | _  -> None in
  match mulop with
  | Some (mulop, s) ->
     let (e, s) = parse_factor s in
     parse_term_aux (acc @ [(mulop, e)]) s
  | None -> (acc, s)

and parse_term es =
  let (e, s) = parse_factor es in
  let (factors, s) = parse_term_aux [] s in
  build_primops e factors, s

and parse_basetype (t, s) =
  match t with
  | INT -> (TInt, s)
  | BOOL -> (TBool, s)
  | LPAREN ->
     let (t, s) = parse_type s in
     let s = tok_match RPAREN s in
     (t, s)
  | _ -> raise (Error "expect a type")

and parse_type_tup s =
  let (t, s) = parse_type s in
  let (n, s') = next_msg s "expect a tuple type" in
  if n = TIMES then
    let (ts, s) = parse_type_tup s' in
    (t :: ts, s)
  else ([t], s)

and parse_type s =
  let (t, s) = next_msg s "expect a base type" in
  let (domain, s) = parse_basetype (t, s) in
  let (n, s') = next_msg s "expect ->, *, or nothing" in
  match n with
  | ARROW ->
     let (range, s) = parse_type s' in
     (TArrow (domain, range), s)
  | TIMES ->
     let (ts, s) = parse_type_tup s' in
     (TProduct (domain :: ts), s)
  | _ -> (domain, s)

let parse_exp_semi s =
  let (e, s) = parse_exp s in
  let s = tok_match SEMICOLON s in
  (e, s)

let parse_exps s =
  let rec helper s =
    try
      let (e, s) = parse_exp_semi s in
      let (es, s) = helper s in
      (e :: es, s)
    with
      Error _ -> ([], s)
  in
  let (e, s) = parse_exp_semi s in
  let (es, s) = helper s in
  (e :: es, s)

let parse lexer lexbuf =
  let s = lexer_to_stream lexer lexbuf in
  let (e, s) = parse_exp_semi s in
  ignore (tok_match EOF s);
  e
