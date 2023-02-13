

(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points) *)
(* ------------------------------------------------------------------------*)

let new_account p =
  (* Set up references to be used in all these functions*)
  let pass = ref p in
  let balance = ref 0 in
  let attempts = ref 0 in
  (*  Helper methods : manage password entering and associated exceptions.
      Only if the password is correct and we have not passed the limit,
      is the argument f called. *)
  let check_pass pwd f =
    if pwd = !pass then
      (attempts := 0; f ())
    else
      (* incr increases the value stored in an int ref by 1 *)
      (incr attempts; raise wrong_pass)
  in
  let attempt pwd f =
    if !attempts >= 3 then
      raise too_many_attempts
    else
      check_pass pwd f
  in
  (* The actual commands, all wrapped under check_pass or attempt *)
  let update_passwd old new_p =
    check_pass old (fun () -> pass := new_p) in
  let deposit pwd amt =
    attempt pwd (fun () -> balance := !balance + amt) in
  let print_balance pwd =
    attempt pwd (fun () -> !balance) in
  let retrieve pwd amt =
    attempt pwd (fun () ->
        if !balance >= amt then balance := !balance - amt
        else raise no_money) in
  (* Final assembly : usually records require to write {k1 = v1; k2 = v2};
      However, if the expression passed has the key's name and type,
      it is done automatically *)
  {update_passwd; deposit; print_balance; retrieve}




(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)
(* Fib *)


(* Q 2.1 : Counting how many function calls are made *)
let rec fib_I n =
  let c = ref 0 in
  let rec fib n =
    (incr c;
     match n with
     | 0 -> 0
     | 1 -> 1
     | n -> fib (n-2) + fib (n-1))
  in
    { num_rec = !c; result = fib n }


(* Q 2.2 Memoization with a global store *) 
let fib_memo n =  
  let rec fib n =
    match Hashtbl.find_opt store n with
      | Some v -> v
      | None ->
        match n with
        | 0 -> (Hashtbl.add store n 0; 0)
        | 1 -> (Hashtbl.add store n 1; 1)             
        | n ->
          let b = fib (n-2) + fib (n-1)
          in
            (Hashtbl.add store n b; b)
  in
    fib n

(* Q 2.3 General memoization function *)

let memo f stats =
  let hash = Hashtbl.create 1000 in
  let rec f' a =
    match Hashtbl.find_opt hash a with
    | Some v -> incr stats.lkp; v
    | None ->
      let b = f f' a in
      incr stats.entries;
	    Hashtbl.add hash a b;
      b
  in
  f'


(* Q 2.4 : Using memo to efficiently compute the fib*)

let fib_memoize (f:int -> int) n = match n with   
    | 0  -> 0 
    | 1  ->  1
    | n  ->  (f (n - 1)) +  (f (n - 2))

let fibM  =
  let s = { lkp = ref 0; entries = ref 0 } in
  let mf = memo fib_memoize s in
  (fun n -> (mf n, s))

