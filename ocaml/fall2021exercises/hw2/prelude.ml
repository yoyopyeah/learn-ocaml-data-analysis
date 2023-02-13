exception NotImplemented
exception Error

(* Q1 The type of nucleobase. *)
type nucleobase = T | A | C | G


(* Q2 *)
type exp = 
  | PLUS  of exp * exp  (* Plus *)
  | MINUS of exp * exp  (* Minus *)
  | MULT of exp * exp  (* Mult *)
  | DIV   of exp * exp  (* Div *)
  | SIN   of exp        (* Sin *)
  | COS   of exp        (* Cos *)
  | EXP   of exp        (* Exp *)
  | FLOAT of float


type instruction = Plus | Minus | Mult | Div | Sin | Cos | Exp | Float of float

type stack = float list