open Work

(** Maps a function over an option. *)
let omap f = function
  | Some x -> Some (f x)
  | None -> None

(** Finds the index of the specified element in a list. *)
let rec index_of y = function
  | [] -> None
  | x :: xs when x = y -> Some 0
  | x :: xs -> omap (fun i -> i + 1) @@ index_of y xs

let rec lrepeat x n =
  if n <= 0 then []
  else x :: lrepeat x (n - 1)

exception Impossible

(** Converts an expression to de Bruijn index form, using the given context.
    The context is used to determine what index a variable should be
    associated with, and grows for recursive calls under binders.

we use "$n" as a special form for names of debruijn indices.
 *)
let rec bruijnify_exp ctx = function
  | Var x ->
     begin
       match index_of x ctx with
       | Some i -> Var ("$" ^ string_of_int i)
       | None -> Var x
     end
  | Int x -> Int x
  | Bool x -> Bool x
  | If (e, e1, e2) ->
     If (bruijnify_exp ctx e, bruijnify_exp ctx e1, bruijnify_exp ctx e2)
  | Primop (op, es) -> Primop (op, List.map (bruijnify_exp ctx) es)
  | Fn (x, t, e) ->
     (* we don't give a name here because de Bruijn indices has one less field *)
      Fn ("", t, bruijnify_exp (x :: ctx) e)
  | Rec (x, t, e) ->
     Rec ("", t, bruijnify_exp (x :: ctx) e)
  | Let ([], e2) -> Let ([], bruijnify_exp ctx e2)
  | Let (d :: decs, e2) ->
     begin match d with
     | Val (e, x) ->
        begin match bruijnify_exp (x :: ctx) (Let (decs, e2)) with
        | Let (ds', e2') -> Let (Val (bruijnify_exp ctx e, "") :: ds', e2')
        | _ -> raise Impossible
        end
     | ByName (e, x) ->
        begin match bruijnify_exp (x :: ctx) (Let (decs, e2)) with
        | Let (ds', e2') -> Let (ByName (bruijnify_exp ctx e, "") :: ds', e2')
        | _ -> raise Impossible
        end
     | Valtuple (e, xs) ->
        begin match bruijnify_exp (List.rev xs @ ctx) (Let (decs, e2)) with
        | Let (ds', e2') -> Let (Valtuple (bruijnify_exp ctx e, lrepeat "" (List.length xs)) :: ds', e2')
        | _ -> raise Impossible
        end
     end
  | Apply (e1, e2) ->
     Apply (bruijnify_exp ctx e1, bruijnify_exp ctx e2)
  | Tuple es ->
     Tuple (List.map (bruijnify_exp ctx) es)
  | Anno (e, t) ->
     Anno (bruijnify_exp ctx e, t)

let compare_exp e1 e2 =
  let e1' = bruijnify_exp [] e1 in
  let e2' = bruijnify_exp [] e2 in
  e1' = e2'

let unsafe_parse s =
  match P.parse s with
  | Right e -> e
  | Left _ -> raise Impossible

let set_incl l1 l2 =
  List.for_all (fun x -> List.mem x l2) l1

let set_eq l1 l2 = set_incl l1 l2 && set_incl l2 l1

let random_char () =
  let i = Random.int 52 in
  if i < 26 then
    Char.chr (65 + i)           (* upper case *)
  else
    Char.chr (97 - 26 + i)      (* lower case *)

let random_name sz =
  String.init (sz + 2) (fun _ -> random_char ())

(** we can shuffle the name of an exp so that students' algorithms are name independent *)
let rec shuffle_name ctx = function
  | Var x ->
     begin match List.find_opt (fun (n, _) -> x = n) ctx with
     | None -> Var x
     | Some (_, i) -> Var i
     end
  | Int x -> Int x
  | Bool x -> Bool x
  | If (e, e1, e2) ->
     If (shuffle_name ctx e, shuffle_name ctx e1, shuffle_name ctx e2)
  | Primop (op, es) -> Primop (op, List.map (shuffle_name ctx) es)
  | Fn (x, t, e) ->
     (* we don't give a name here because de Bruijn indices has one less field *)
     let name = random_name (String.length x) in
     Fn (name, t, shuffle_name ((x, name) :: ctx) e)
  | Rec (x, t, e) ->
     let name = random_name (String.length x) in
     Rec (name, t, shuffle_name ((x, name) :: ctx) e)
  | Let ([], e2) -> Let ([], shuffle_name ctx e2)
  | Let (d :: decs, e2) ->
     begin match d with
     | Val (e, x) ->
        begin match List.find_opt (fun (n, _) -> x = n) ctx with
        | None ->
           let name = random_name (String.length x) in
           begin match shuffle_name ((x, name) :: ctx) (Let (decs, e2)) with
           | Let (ds', e2') -> Let (Val (shuffle_name ctx e, name) :: ds', e2')
           | _ -> raise Impossible
           end
        | Some (_, i) ->
           begin match shuffle_name ctx (Let (decs, e2)) with
           | Let (ds', e2') -> Let (Val (shuffle_name ctx e, i) :: ds', e2')
           | _ -> raise Impossible
           end
        end
     | ByName (e, x) ->
        begin match List.find_opt (fun (n, _) -> x = n) ctx with
        | None ->
           let name = random_name (String.length x) in
           begin match shuffle_name ((x, name) :: ctx) (Let (decs, e2)) with
           | Let (ds', e2') -> Let (ByName (shuffle_name ctx e, name) :: ds', e2')
           | _ -> raise Impossible
           end
        | Some (_, i) ->
           begin match shuffle_name ctx (Let (decs, e2)) with
           | Let (ds', e2') -> Let (ByName (shuffle_name ctx e, i) :: ds', e2')
           | _ -> raise Impossible
           end
        end
     | Valtuple (e, xs) ->
        let rec getRandomNames xs =
          match xs with
          | [] -> ([], [])
          | x :: xs ->
             match List.find_opt (fun (n, _) -> x = n) ctx with
             | None ->
                let n = random_name (String.length x) in
                let (allNs, newNs) = getRandomNames xs in
                (n :: allNs, (x, n) :: newNs)
             | Some (_, i) ->
                let (allNs, newNs) = getRandomNames xs in
                (i :: allNs, newNs)
        in
        let (allNames, newNameMaps) = getRandomNames xs in
        begin match shuffle_name (newNameMaps @ ctx) (Let (decs, e2)) with
        | Let (ds', e2') -> Let (Valtuple (shuffle_name ctx e, allNames) :: ds', e2')
        | _ -> raise Impossible
        end
     end
  | Apply (e1, e2) ->
     Apply (shuffle_name ctx e1, shuffle_name ctx e2)
  | Tuple es ->
     Tuple (List.map (shuffle_name ctx) es)
  | Anno (e, t) ->
     Anno (shuffle_name ctx e, t)

type 'a expect = Output of 'a | TyError | StuckError

let expect_map f = function
  | Output x -> Output (f x)
  | TyError -> TyError
  | StuckError -> StuckError

let match_exn result ex =
  match result, ex with
  | TyError, TypeError _ -> true
  | TyError, NotFound -> true
  | StuckError, Stuck _ -> true
  | _ -> false

let sum_ints = List.fold_left (fun a b -> a + b) 0

let sum_floats = List.fold_left (fun a b -> a +. b) 0.

let report = ref true
let verbose = ref false

let flagged_print f s = if !f then print_endline s

let report_print = flagged_print report
let verbose_print = flagged_print verbose

let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
    if acc = "" then
      el_to_string el
    else
      acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts ignored cmp stringify =
  let scores = List.mapi
                 begin fun idx (input, expected_output) ->
                 if List.mem idx ignored then 0 else
                   begin
                     verbose_print (Format.sprintf "running %s test %d" name idx);
                     try
                       let output = f input in
                       match expected_output with
                       | Output expected ->
                          if not (cmp output expected) then
                            begin
                              verbose_print (name ^ " test #" ^ string_of_int idx ^ " failed");
                              verbose_print (stringify output ^ " <> " ^ stringify expected);
                              0
                            end
                          else
                            1
                       | _ ->
                          verbose_print ("expecting an exception but returns " ^ stringify output);
                          0
                     with
                     | exn ->
                        let matched = match_exn expected_output exn in
                        if not matched then
                          begin
                            verbose_print (name ^ " test #" ^ string_of_int idx ^ " raised an exception:");
                            verbose_print (Printexc.to_string exn);
                            0
                          end
                        else 1
                   end
                 end
                 ts
  in
  sum_ints scores

let rec find_root (t : typ) : typ =
  match t with
  | TVar r ->
     begin match !r with
     | None -> t
     | Some t -> find_root t
     end
  | _ -> t

let rec find_ref r refs =
  match refs with
  | [] -> false
  | x :: xs -> x == r || find_ref r xs

let rec find_base refs (t : typ) : typ option =
  match t with
  | TVar r ->
     if find_ref r refs then None
     else
       begin match !r with
       | None -> Some t
       | Some t -> find_base (r :: refs) t
       end
  | _ -> Some t

let loop_free t : bool =
  let rec helper rfs t =
    match t with
    | TInt | TBool -> true
    | TVar r ->
       if find_ref r rfs then false
       else
         begin match !r with
         | None -> true
         | Some t -> helper (r :: rfs) t
         end
    | TProduct ts -> List.for_all (helper rfs) ts
    | TArrow (d, c) -> helper rfs d && helper rfs c
  in
  helper [] t

(** here we distinguish the inputs. std is the golden source well-formed type while tested is a type
coming from students output which could be error prone. *)
let compare_typ tested std : bool =
  if not (loop_free tested) then false
  else
    let refmap = ref [] in
    let invrefmap = ref [] in
    let rec ref_lookup r refmap =
      match refmap with
      | [] -> None
      | (r', t) :: rs -> if r == r' then Some t else ref_lookup r rs
    in
    let rec helper tested std =
      match find_root tested, find_root std with
      | TInt, TInt
        | TBool, TBool -> true
      | TVar r1, TVar r2 ->
         begin match ref_lookup r1 (!refmap) with
         | None ->
            (* need to make sure r1 and r2 are one to one *)
            begin match ref_lookup r2 (!invrefmap) with
            | None ->
               refmap := (r1, r2) :: !refmap;
               invrefmap := (r2, r1) :: !invrefmap;
               true
            | Some _ -> false
            end
         | Some t -> t == r2 (* not the same as unification. we want to compare them *)
         end
      | TProduct ts1, TProduct ts2 ->
         List.length ts1 = List.length ts2 &&
           List.for_all2 helper ts1 ts2
      | TArrow (d1, c1), TArrow (d2, c2) ->
         helper d1 d2 && helper c1 c2
      | _ -> false
    in
    helper tested std

let test_unify t1 t2 : bool =
  if not (loop_free t1 && loop_free t2) then false
  else
    let rec helper t1 t2 =
      match find_root t1, find_root t2 with
      | TInt, TInt
        | TBool, TBool -> true
      | TVar r1, TVar r2 -> r1 == r2
      | TProduct ts1, TProduct ts2 ->
         List.length ts1 = List.length ts2 &&
           List.for_all2 helper ts1 ts2
      | TArrow (d1, c1), TArrow (d2, c2) ->
         helper d1 d2 && helper c1 c2
      | _ -> false
    in
    helper t1 t2

let grade_question name f ts ignored cmp stringify full_grade =
  let points = run_test name f ts ignored cmp stringify in
  let total = List.length ts in
  let final = float points /. float total *. float full_grade in
  report_print (Format.sprintf "question %s: %d / %d tests, %f / %d points" name points total final full_grade);
  final

let parse_program l = List.map (fun (p, ex) -> unsafe_parse p, ex) l

let q1_progs = [
    "x;", Output ["x"];
    "fn x => x;", Output [];
    "fn x => y;", Output ["y"];
    "let val x = 5 in x end;", Output [];
    "let val x = 5 in y end;", Output ["y"];
    "let val f = fn y => y in f 5 end;", Output [];
    "let val f = fn y : int => fn z : int => y + z in f 5 4 end;", Output [];
    "let val f = fn y => x in f 5 end;", Output ["x"];
    "let val x = x in x end;", Output ["x"];
    "let val x = 5 val y = x in x + y end;", Output [];
    "let val x = 5 val y = z in if true then x else y + w end;", Output ["z"; "w"];
    "let fun f (y : int) : int = y in f 5 end;", Output [];
    "let fun f (y : int) : int = xq fun g (k : int -> int) : int = k (k (4 * (zp - xq))) in g f 5 end;", Output ["xq"; "zp"];
    "let name x = let val yz = xw in yz end in (fn z => x) x end;", Output ["xw"];
    "let val (a, b, c) = (d, a, b) in c end;", Output ["d"; "a"; "b"];
    "let name a = 5 val (a, b) = (5, a) in a end;", Output [];
  ]

let grade_q1 full_grade ignored =
  grade_question "free_vars" free_vars (parse_program q1_progs) ignored set_eq (list_to_string (fun x -> x)) full_grade

let q2_progs = [
    "let fun foo (x : int) : int = x in foo 10 end;", Output [];
    "let fun foo (x : int) : int = 20 in foo 10 end;", Output ["x"];
    "let fun recf (x : int) : int = recf 10 in 20 end;", Output ["recf"; "x"];
    "fn x => y;", Output ["x"];
    "fn z => 30 * z + 20;", Output [];
    "let val ggg = 30 in ggg end;", Output [];
    "let val ggg = 30 in zzz end;", Output ["ggg"];
    "let val zzz = 3   val zzz = 4 in zzz end;", Output ["zzz"];
    "let val xxx = zzz   val zzz = 4 in xxx end;", Output ["zzz"];
    "let val zzz = 3   val ddd = 4 in if true then zzz else ddd end;", Output [];
    "let val yyy = www    val kkk = 20 + (let val zzz = yyy && false in 30 end) in true end;", Output ["kkk"; "zzz"];
    "let val zzz = yyy      val (yyy, zzz, www) = zzz in  www end;", Output ["zzz"; "yyy"];
    "let val (aaa, bbb) = (fn y => z + y, fn y => 20) in bbb 40 end;", Output ["aaa"; "y"];
    "(fn zzz => fn kkk => zzz) 20 30;", Output ["kkk"];
    "(fn www => (fn zzz => 100) www, let name qqq = 30  name eee = 50 in fn rrr => eee - qqq end);", Output ["zzz"; "rrr"]
  ]

let grade_q2 full_grade ignored =
  grade_question "unused_vars" unused_vars (parse_program q2_progs) ignored set_eq (list_to_string (fun x -> x)) full_grade

(* we need to make sure
   1. substitution gets in
   2. renaming happens
   3. probably try out cases where binders clash with the variables being substitued.
 *)
let q3_progs = [
    ("www;", "zzz", "fn zzz => zzz;"),
    Output "fn zzz => zzz;";
    ("www + 20;", "zzz", "fn yyy => zzz;"),
    Output "fn yyy => www + 20;";
    ("www + 20;", "zzz", "fn www => zzz;"),
    Output "fn x => www + 20;";

    ("zzz + www;", "www", "(fn f => fn zzz => f www zzz) (let val www = 20 in www end);"),
    Output "(fn f => fn x => f (zzz + www) x) (let val y = 20 in y end);";

    ("20 * qqq;", "xxx", "let fun foo (yyy : int) : int = xxx + yyy in 30 end;"),
    Output "let fun foo (yyy : int) : int = 20 * qqq + yyy in 30 end;";
    ("20 * qqq;", "foo", "let fun foo (yyy : int) : int = xxx + yyy in 30 end;"),
    Output "let fun foo (yyy : int) : int = xxx + yyy in 30 end;";
    ("20 * qqq;", "xxx", "let fun foo (qqq : int) : int = xxx + qqq in 30 end;"),
    Output "let fun foo (x : int) : int = 20 * qqq + x in 30 end;";

    (* easy case *)
    ("zzz + 10;", "www", "let val kkk = www + 20 in www end;"),
    Output "let val kkk = (zzz + 10) + 20 in zzz + 10 end;";
    ("3;", "ww", "let val aqwe = ww   val beqe = brte + ww  in ww + aqwe + beqe end;"),
    Output "let val aqwe = 3   val beqe = brte + 3  in 3 + aqwe + beqe end;";

    ("zzz;", "xxx", "let val xxx = 20 val yyy = xxx in yyy end;"),
    Output "let val xxx = 20 val yyy = xxx in yyy end;";
    ("zzz;", "xxx", "let val zzz = 20 val yyy = zzz + xxx in yyy + zzz + xxx end;"),
    Output "let val z = 20 val yyy = z + zzz in yyy + z + zzz end;";
    ("zzz;", "xxx", "let name xxx = 20 name yyy = xxx in yyy end;"),
    Output "let name xxx = 20 name yyy = xxx in yyy end;";
    ("zzz;", "xxx", "let name zzz = 20 name yyy = zzz + xxx in yyy + zzz + xxx end;"),
    Output "let name z = 20 name yyy = z + zzz in yyy + z + zzz end;";

    ("zzz;", "xxx", "let val (zzz, www) = (20, true) name yyy = zzz + xxx in yyy + zzz + xxx end;"),
    Output "let val (z, www) = (20, true) name yyy = z + zzz in yyy + z + zzz end;";
    ("zzz;", "xxx", "let val (zzz, xxx) = (20, true) name yyy = zzz + xxx in yyy + zzz + xxx end;"),
    Output "let val (zzz, xxx) = (20, true) name yyy = zzz + xxx in yyy + zzz + xxx end;"
  ]

let parse_q3_progs = List.map (fun ((e', x, e), ex) -> ((unsafe_parse e', x, unsafe_parse e), expect_map unsafe_parse ex))

let grade_q3 full_grade ignored =
  grade_question "subst" (fun (e', x, e) -> subst (e', x) e) (parse_q3_progs q3_progs) ignored compare_exp Print.exp_to_string full_grade


let q4_progs = [
    (* functions *)
    "(fn x => x);", Output (Fn ("x", None, Var "x"));
    "(fn x => let val x = 5 in x end);", Output (Fn ("x", None, Let ([Val (Int 5, "x")], Var "x")));
    "(fn x => let val x = 5 + 4 in x end);", Output (Fn ("x", None, Let ([Val (Primop (Plus, [Int 5; Int 4]), "x")], Var "x")));

    (* application *)
    "(fn f => f 3) 10;", StuckError;
    "(fn x => x + x) 10;", Output (Int 20);
    "(fn x => x = 30) 30;", Output (Bool true);
    "(fn f => fn x => f (f x)) (fn x => x + 1) 0;", Output (Int 2);
    "(fn f => fn x => f (f (f (f x)))) ((fn f => fn x => f (f (f x))) (fn x => x + 1)) 0;", Output (Int 12);
    "(fn f => fn x => f (f (f (f x)))) (fn f => fn x => f (f (f x))) (fn x => x + 1) 0;", Output (Int 81);
    "(fn f => fn x => f (f x)) (fn y => y);", Output (Fn ("x", None, Apply (Fn ("y", None, Var "y"), Apply (Fn ("y", None, Var "y"), Var "x"))));

    (* and/or *)
    "true && 5;", Output (Int 5);
    "false || 10;", Output (Int 10);
    "10 || 5 = 10;", StuckError;
    "(fn x => x) && 10;", StuckError;

    (* let expression *)
    "let val x = 4 in x end;", Output (Int 4);
    "let val (a, b) = (4, 3) in a * b end;", Output (Int 12);
    "let name x = 23 + 3 in x end;", Output (Int 26);
    "let val x = 4 val y = x in y end;", Output (Int 4);
    "let name z = z in 5 + z end;", StuckError;
    "let val (a, b) = (27, a) in a / b end;", StuckError;
    "let val x = 4 val x = x + 3 in x end;", Output (Int 7);
    "let val x = 4 name x = let name x = x + 7 in x end in x - 2 end;", Output (Int 9);
    "let val double = fn x => x + x in double 5 end;", Output (Int 10);
    "let val double = fn x => if x = 0 then 0 else 2 + double (x - 1) in double 5 end;", StuckError;
    "let fun repeat (x : int) : (int * int) = (x, x) val (a, b) = repeat 5 in a - b end;", Output (Int 0);
    "let fun add (x : int * int) : int = let val (a, b) = x in a + b end val x = add (4, 3) in x end;", Output (Int 7);
    "let fun fib (x : int) : int = if x < 2 then x else fib (x - 1) + fib (x - 2) in fib 6 end;", Output (Int 8);
    (* and/or short-circuit *)
    "let val x = 4 fun isNonnegativeEven (x : int) : bool = x = 0 || (x != 1 && isNonnegativeEven (x - 2)) in isNonnegativeEven (x - 1) end;", Output (Bool false);
    "let val isPrime = fn x => let fun helper (n : int) : bool = n * n >= x || ((x / n) * n != x && helper (n + 1)) in helper 2 end val (ip5, ip6) = (isPrime 5, isPrime 6) val ip7 = isPrime 7 in (ip5, ip6) end;", Output (Tuple [Bool true; Bool false]);

    (* check val implementation *)
    "let val x0 = 0 + 0 + 0 + 0 val x1 = x0 + x0 + x0 + x0 val x2 = x1 + x1 + x1 + x1 val x3 = x2 + x2 + x2 + x2 val x4 = x3 + x3 + x3 + x3 val x5 = x4 + x4 + x4 + x4 val x6 = x5 + x5 + x5 + x5 val x7 = x6 + x6 + x6 + x6 val x8 = x7 + x7 + x7 + x7 val x9 = x8 + x8 + x8 + x8 val x10 = x9 + x9 + x9 + x9 val x11 = x10 + x10 + x10 + x10 val x12 = x11 + x11 + x11 + x11 val x13 = x12 + x12 + x12 + x12 val x14 = x13 + x13 + x13 + x13 val x15 = x14 + x14 + x14 + x14 val x16 = x15 + x15 + x15 + x15 val x17 = x16 + x16 + x16 + x16 val x18 = x17 + x17 + x17 + x17 val x19 = x18 + x18 + x18 + x18 val x20 = x19 + x19 + x19 + x19 val x21 = x20 + x20 + x20 + x20 val x22 = x21 + x21 + x21 + x21 val x23 = x22 + x22 + x22 + x22 val x24 = x23 + x23 + x23 + x23 val x25 = x24 + x24 + x24 + x24 val x26 = x25 + x25 + x25 + x25 val x27 = x26 + x26 + x26 + x26 val x28 = x27 + x27 + x27 + x27 val x29 = x28 + x28 + x28 + x28 val x30 = x29 + x29 + x29 + x29 val x31 = x30 + x30 + x30 + x30 in x31 + x31 + x31 + x31 end;", Output (Int 0);

    (* check name implementation *)
    "let fun loop (x : int) : int = loop x name y = loop 0 in if true then 5 else y end;", Output (Int 5);
  ]

let grade_q4 full_grade ignored =
  grade_question "eval" (fun e -> eval (shuffle_name [] e)) (parse_program q4_progs) ignored compare_exp Print.exp_to_string full_grade


let q5_progs = [
    ([], "1;"), Output TInt;
    ([], "true;"), Output TBool;

    (* if expression *)
    (["xxx", TBool; "yyy", TInt; "zzz", TInt], "if xxx then yyy else zzz;"), Output TInt;
    (["xxx", TInt; "yyy", TInt; "zzz", TInt], "if xxx then yyy else zzz;"), TyError;
    (["xxx", TBool; "yyy", TArrow (TInt, TInt); "zzz", TInt], "if xxx then yyy else zzz;"), TyError;

    (* operators *)
    (["zzz", TBool], "zzz && false;"), Output TBool;
    (["zzz", TBool], "zzz && 3;"), TyError;
    (["xxx", TInt], "1 + xxx * 3;"), Output TInt;
    (["xxx", TBool], "1 + xxx * 3;"), TyError;
    (["www", TInt], "1 + ~ www;"), Output TInt;
    (["www", TArrow (TBool, TInt)], "1 + ~ www;"), TyError;

    (* functions *)
    (["zzz", TInt], "fn www : int => zzz / www;"), Output (TArrow (TInt, TInt));
    (["fff", TArrow (TArrow (TInt, TInt), TInt)], "(fn ggg : int -> int => 20 - fff ggg) (fn xxx : int => xxx);"),
    Output TInt;

    (["xxx", TInt], "let fun foo (yyy : int) : int = xxx + yyy in foo end;"),
    Output (TArrow (TInt, TInt));
    (["xxx", TInt; "yyy", (TArrow (TInt, TInt))],
     "let val zzz = xxx * yyy xxx   val qqq = (zzz, yyy zzz = zzz)  fun fff (ppp : bool) : (bool * (int * bool)) = let val (www, yyy) = qqq in (ppp, (www, yyy)) end in fff end;"),
    Output (TArrow (TBool, TProduct [TBool; TProduct [TInt; TBool]]));

    ([], "let val xxx = 1  name xxx = true in xxx end;"),
    Output TBool;
    (["fff", TArrow (TInt, TInt)], "let val xxx = fn zzz : int => fff zzz  val (qqq, xxx) = (xxx, xxx) in xxx (qqq 20) end;"),
    Output TInt;
    (["fff", TArrow (TInt, TInt)], "let fun xxx (zzz : int) : int = fff zzz  val (qqq, xxx) = (xxx, xxx) in xxx (qqq 20) end;"),
    Output TInt;
    (["zzz", TProduct [TArrow (TInt, TInt); TInt]], "let val (xxx, yyy) = zzz val xxx = xxx yyy in xxx end;"),
    Output TInt;
    (["yyy", TBool], "let val xxx = let val yyy = 30 in 10 end in yyy end;"),
    Output TBool;

    (* S combinator *)
    ([], "fn xxx : int -> int -> int => fn yyy : int -> int => fn zzz : int => xxx zzz (yyy zzz);"),
    Output (TArrow (TArrow (TInt, TArrow (TInt, TInt)), TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt))));
    (["www", TProduct [TInt; TInt]], "let fun apply (f : int -> int) : int -> int =
                                      fn x : int => f(x) val (xxx, zzz) = www in apply (fn x : int => x * zzz) xxx end; "),
    Output TInt;
    ([], "let fun fib (x : int) : int = if x <= 1 then 1 else fib (x - 1) + fib (x - 2) in fib 20 end;"),
    Output TInt;

    ([], "let fun even (x : int) : bool = if x < 0 then false else if x = 0 then true else if x = 1 then false else even (x - 2) in even end;"),
    Output (TArrow (TInt, TBool));
    ([], "let fun even (x : int) : bool = if x < 0 then false else if x = 0 then true else if x = 1 then false else even (x - 2)
          fun collatz (x : int) : bool = if x <= 1 then true else if even x then collatz (x / 2) else collatz (3 * x + 1) in collatz 3 end;"),
    Output TBool;
  ]

let parse_q5_progs = List.map (fun ((ctx, e), ex) -> ((Ctx ctx, unsafe_parse e), ex))

let grade_q5 full_grade ignored =
  grade_question "type check" (fun (ctx, e) -> infer ctx e) (parse_q5_progs q5_progs) ignored compare_typ Print.typ_to_string full_grade

let q6_progs = [
    (* simple cases *)
    ((TInt, TInt), Output ());
    ((TBool, TInt), TyError);
    (TArrow (TInt, TBool), TProduct [TInt; TBool]), TyError;
    (TProduct [TInt; TBool; TInt], TProduct [TInt; TBool]), TyError;
    (* tvar unifies *)
    ((TArrow (TInt, TInt), fresh_tvar ()), Output ());
    ((TInt, TVar (ref (Some TInt))), Output ());
    ((TInt, TVar (ref (Some (fresh_tvar ())))), Output ());
    ((TInt, TVar (ref (Some TBool))), TyError);
    ((TVar (ref (Some (TVar (ref (Some TInt))))), TVar (ref (Some (TVar (ref (Some TInt)))))), Output ());
    ((TVar (ref (Some (TVar (ref (Some TInt))))), TVar (ref (Some (TVar (ref (Some TBool)))))), TyError);
    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TProduct [a; TBool], TProduct [TInt; b]), Output ());

    (* nested tvars *)
    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TArrow (a, b), TArrow (b, a)), Output ());
    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TArrow (a, (TArrow (a, b))), TArrow (TInt, (TArrow (b, a)))), Output ());
    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TProduct [TVar (ref (Some (fresh_tvar ()))); a], TProduct [a; TProduct [b; TBool]]), Output ());

    (let a = fresh_tvar () in
     ((a, a), Output ()));
    (let a = fresh_tvar () in
     ((TVar (ref (Some a)), a), Output ()));
    (let a = fresh_tvar () in
     ((TVar (ref (Some a)), TVar (ref (Some TBool))), Output ()));
    (let a = fresh_tvar () in
     ((a, TArrow (a, a)), TyError));
    (let a = fresh_tvar () in
     ((a, TArrow (TInt, TVar (ref (Some a)))), TyError));
    (let a = fresh_tvar () in
     ((a, TProduct [a; TInt]), TyError));
    (let a = fresh_tvar () in
     ((a, TProduct [TVar (ref (Some a)); TInt]), TyError));
    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TArrow (a, (TArrow (a, b))), TArrow (b, a)), TyError);
    (let a = fresh_tvar () in
     (TProduct [TVar (ref (Some a)); TBool], a), TyError);

    (let a = fresh_tvar ()
     and b = fresh_tvar ()
     and c = fresh_tvar () in
     (TProduct [a; TArrow (c, c)], TProduct [b; b]), Output ());
    (let a = fresh_tvar ()
     and b = fresh_tvar ()
     and c = fresh_tvar () in
     (TProduct [a; TArrow (b, c)], TProduct [b; b]), TyError);

    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TVar (ref (Some (TArrow (a, a)))), TVar (ref (Some (TArrow (b, TVar (ref (Some (TArrow (a, a))))))))), TyError);
    (let a = fresh_tvar ()
     and b = fresh_tvar () in
     (TVar (ref (Some (TProduct [a; a]))), TVar (ref (Some (TProduct [b; TVar (ref (Some (TProduct [a; a])))])))), TyError);
    (let a = fresh_tvar ()
     and b = fresh_tvar ()
     and c = fresh_tvar () in
     (TProduct [a; b; a], TProduct [b; TArrow (c, c); TInt]), TyError);
    (let a = fresh_tvar ()
     and b = fresh_tvar ()
     and c = fresh_tvar () in
     (TProduct [a; b; a], TProduct [b; TArrow (c, c); TInt]), TyError);
    (let a = fresh_tvar ()
     and b = fresh_tvar ()
     and c = fresh_tvar () in
     (TProduct [a; b; a], TProduct [TArrow (b, c); TInt; c]), TyError);

  ]


let grade_q6 full_grade ignored =
  let points = List.mapi
                 begin fun idx ((t1, t2), expected_output) ->
                 if List.mem idx ignored then 0 else
                   begin
                     verbose_print (Format.sprintf "running unification test %d" idx);
                     try
                       let output = unify t1 t2 in
                       match expected_output with
                       | Output expected ->
                          if not (test_unify t1 t2) then
                            begin
                              verbose_print ("unification test #" ^ string_of_int idx ^ " failed");
                              0
                            end
                          else
                            1
                       | _ ->
                          verbose_print ("expecting an exception but unification succeeded");
                          0
                     with
                     | exn ->
                        let matched = match_exn expected_output exn in
                        if not matched then
                          begin
                            verbose_print ("unification test #" ^ string_of_int idx ^ " raised an exception:");
                            verbose_print (Printexc.to_string exn);
                            0
                          end
                        else 1
                   end
                 end
                 q6_progs
  in
  let points = sum_ints points in
  let total = List.length q6_progs in
  let final = float points /. float total *. float full_grade in
  report_print (Format.sprintf "question unification: %d / %d tests, %f / %d points" points total final full_grade);
  final
  (* grade_question "unification" (fun (t1, t2) -> unify t1 t2) q6_progs ignored (fun _ _ -> true) (fun _ -> "()") full_grade *)

let q7_progs = [
    ([], "fn x => x + 1;"), Output (TArrow (TInt, TInt));
    (([], "fn x => x;"), let a = fresh_tvar () in Output (TArrow (a, a)));
    (([], "fn f => fn x => (fn x => x) f x;"),
     let a = fresh_tvar ()
     and b = fresh_tvar ()
     in Output (TArrow (TArrow (a, b), TArrow (a, b))));
    (([], "let val id = fn x => x  val x = id 1 in id end;"),
     Output (TArrow (TInt, TInt)));
    (([], "let val foo = fn x => let val (y, z) = x in y + z end in foo end;"),
     Output (TArrow (TProduct [TInt; TInt], TInt)));
    (([], "let val foo = fn f => fn x => let val y = f 1  val (z, w) = x in f z end in foo end;"),
     let a = fresh_tvar ()
     and b = fresh_tvar ()
     in Output (TArrow (TArrow (TInt, a), TArrow (TProduct [TInt; b], a))));
    (([], "let val swap = fn x => let val (y, z) = x in (z, y) end   val foo = fn f => fn x => swap (f x) in (swap, foo) end;"),
     let a = fresh_tvar ()
     and b = fresh_tvar ()
     and c = fresh_tvar ()
     in Output (TProduct [TArrow (TProduct [a; b], TProduct [b; a]); TArrow (TArrow (c, TProduct [a; b]), TArrow (c, TProduct [b; a]))]));
    (([], "let val foo = fn f => fn x => let val y = f 1  val (z, w) = x in f w end in foo end;"),
     let a = fresh_tvar ()
     and b = fresh_tvar ()
     in Output (TArrow (TArrow (TInt, a), TArrow (TProduct [b; TInt], a))));
    (([], "let val foo = fn f => fn g => let val x = g 1 in f g end in foo end;"),
     let a = fresh_tvar ()
     and b = fresh_tvar ()
     in Output (TArrow (TArrow (TArrow (TInt, a), b), TArrow (TArrow (TInt, a), b))));
    (([], "let val foo = fn f => fn x => let val (z, w) = x  val q = f z  val y = f q in w end in foo end;"),
     let a = fresh_tvar ()
     and b = fresh_tvar ()
     in Output (TArrow (TArrow (a, a), TArrow (TProduct [a; b], b))));
  ]

let grade_q7 full_grade ignored =
  grade_question "infer" (fun (ctx, e) -> infer ctx e) (parse_q5_progs q7_progs) ignored compare_typ Print.typ_to_string full_grade

let full_grades = [
    10;
    15;
    15;
    30;
    25;
    30;
    10;
  ]

let graders = [
    grade_q1;
    grade_q2;
    grade_q3;
    grade_q4;
    grade_q5;
    grade_q6;
    grade_q7;
  ]

let total_questions = List.length graders

let bonus = [7]

let parse_args () =
  let parse_comma_separated s = List.map int_of_string (String.split_on_char ',' s) in
  let ignoreds = Array.init total_questions (fun _ -> ref []) in
  let runs = ref (Array.to_list (Array.init total_questions (fun i -> i + 1))) in
  let args = [
      "-verbose", Arg.Set verbose, "verbose printing";
      "-report", Arg.Clear report, "disable report printing";
      "-run-only", Arg.String (fun s -> runs := parse_comma_separated s), "run only comma specified tests";
    ] @ Array.to_list (Array.init total_questions
                                  (fun i -> let i = i + 1 in
                                            Format.sprintf "-q%dignored" i,
                                            Arg.String (fun s -> ignoreds.(i - 1) := parse_comma_separated s),
                                            Format.sprintf "ignoring comma separated tests in q%d" i)) in
  Arg.parse args (fun _ -> ()) "grade the student";
  !runs, Array.map (fun r -> !r) ignoreds


let main () =
  let runs, ignored = parse_args () in
  let rec filter_i i l =
    match l with
    | [] -> []
    | x::l -> if List.mem i runs && not (List.mem i bonus) then x :: filter_i (i + 1) l else filter_i (i + 1) l
  in
  let grades = List.mapi (fun i (grader, fg) ->
                   let i = i + 1 in
                   if List.mem i runs then
                     begin
                       verbose_print ("running tests for question " ^ string_of_int i);
                       grader fg ignored.(i - 1)
                     end
                   else
                     0.
                 ) (List.combine graders full_grades) in
  Format.printf "%f / %d\n" (sum_floats grades) (sum_ints (filter_i 1 full_grades))

let () = main ()
