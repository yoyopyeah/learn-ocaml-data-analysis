(* TODO: Write a good set of tests for compress and decompress. *)
let compress_tests = [
  ([], []);
  ([A], [(1, A)]);
  ([A;A], [(2, A)]);
  ([A;G;G;A;T;G;T;C], [(1, A); (2, G); (1, A); (1, T); (1, G); (1, T); (1, C)]);
  ([A;A;G;G;G;A;T;T;T;T;G;C], [(2, A); (3, G); (1, A); (4, T); (1, G); (1, C)]);
  ([A;G], [(1, A); (1, G)]);
]

let decompress_tests = [
  ([], []);
  ([(1, A)], [A]);
  ([(2, A)], [A;A]);
  ([(1, A); (2, G); (1, A); (1, T); (1, G); (1, T); (1, C)] , [A;G;G;A;T;G;T;C]);
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  let rec get_head h n =
    function
    | []               -> (n, h), []
    | x::xs when x = h -> get_head h (n+1) xs
    | xs               -> (n, h), xs
 in
 let rec compress =
   function
   | []    -> []
   | x::xs ->
      let x', xs' = get_head x 1 xs in
      x' :: compress xs'
 in
 compress l

(* TODO: Implement compress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec repeat h =
    function
    | 0 -> []
    | n -> h :: repeat h (n - 1)
  in
  match l with
  | []         -> []
  | (n, h)::xs -> repeat h n @ decompress xs




(* ------------------------------------------------------------*)
(* QUESTION : Pocket Calculator                                *)
(* ------------------------------------------------------------*)

let eval_tests = [
  (PLUS (FLOAT 1., FLOAT 1.), 2.);
  (MINUS (FLOAT 1., FLOAT 1.), 0.);
  (MULT (FLOAT 1., FLOAT 1.), 1.);
  (DIV (FLOAT 1., FLOAT 1.), 1.);
  (COS (FLOAT 0.), 1.);
  (SIN (FLOAT 0.), 0.);
  (EXP (FLOAT 0.), 1.);
  ((FLOAT 1.), 1.);
]

let rec eval e =
  match e with
  | PLUS (e1, e2)  -> eval e1 +. eval e2
  | MINUS (e1, e2) -> eval e1 -. eval e2
  | MULT (e1, e2)  -> eval e1 *. eval e2
  | DIV (e1, e2)   -> eval e1 /. eval e2
  | SIN e          -> sin (eval e)
  | COS e          -> cos (eval e)
  | EXP e          -> exp (eval e)
  | FLOAT n        -> n



let to_instr_tests = [
  ((PLUS (FLOAT 2.2, FLOAT 3.3)), [Float 2.2; Float 3.3; Plus]);
  ((MINUS (FLOAT 3., FLOAT 2.)), [Float 3.; Float 2.; Minus]);
  ((MULT (FLOAT 1., FLOAT 2.)), [Float 1.; Float 2.; Mult]);
  ((DIV (FLOAT 2., FLOAT 1.)), [Float 2.; Float 1.; Div]);
  ((SIN (FLOAT 1.)), [Float 1.; Sin]);
  ((COS (FLOAT 1.)), [Float 1.; Cos]);
  ((EXP (FLOAT 1.)), [Float 1.; Exp]);
  ((FLOAT 1.), [Float 1.]);
]

let rec to_instr e =
  match e with
  | PLUS (e1, e2)  -> to_instr e1 @ to_instr e2 @ [Plus]
  | MINUS (e1, e2) -> to_instr e1 @ to_instr e2 @ [Minus]
  | MULT (e1, e2)  -> to_instr e1 @ to_instr e2 @ [Mult]
  | DIV (e1, e2)   -> to_instr e1 @ to_instr e2 @ [Div]
  | SIN e          -> to_instr e @ [Sin]
  | COS e          -> to_instr e @ [Cos]
  | EXP e          -> to_instr e @ [Exp]
  | FLOAT n        -> [Float n]




(* TODO: Write a good set of tests for prog. *)
let instr_tests = [
  ((Plus, [1.; 1.]), (Some [2.]) );
  ((Minus, [1.; 3.]), (Some [2.]));
  ((Cos, [0.]), (Some [1.]));
  ((Float 1., [1.]), (Some [1.; 1.]));
]

(* Return an optional for instr and prog *)
let instr i s =
  match i, s with
  | Plus   , x::y::s -> Some (y +. x :: s)
  | Minus  , x::y::s -> Some (y -. x :: s)
  | Mult   , x::y::s -> Some (y *. x :: s)
  | Div    , x::y::s -> Some(y /. x :: s)
  | Sin    ,    x::s -> Some (sin x :: s)
  | Cos    ,    x::s -> Some (cos x :: s)
  | Exp    ,    x::s -> Some (exp x :: s)
  | Float n,       s -> Some (n :: s)
  | _      ,       _ -> None




let prog_tests = [
  ([Float 1.; Float 1.; Plus], (Some 2.));
  ([Float 3.; Float 1.; Minus], (Some 2.));
  ([Float 3.; Float 1.; Div], (Some 3.));
  ([Float 0.; Sin], (Some 0.));
  ([Float 0.; Float 0.; Plus; Cos], (Some 1.));
]

let prog instrs =
  let rec aux i_list s =
    match i_list with
    | i::instrs -> begin
                     match instr i s with
                     | Some res -> aux instrs res
                     | None     -> None
                   end
    | []        -> Some s
  in match aux instrs [] with
    | Some s -> begin
                  match s with
                  | [r] -> Some r
                  | _   -> None
                end
    | None   -> None
