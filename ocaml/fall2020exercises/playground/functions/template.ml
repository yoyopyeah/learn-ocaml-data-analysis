(* Code for Lecture 1: Binding, Scope, Functions *)
(* Bindings *)
(* Simplest form of a binding is a declaration. *)

let pi = 3.14 ;;

(* pi is defined up to 2 digits;
   we say we bind the variable name "pi" to the value 3.14

   NOTE:
   - a variable binding is not an assignment.
   - we can only *overshadow* a previous binding, but the previous
     variable is not updated.
   - Variable bindings once introduced persist.
   *)
(* We also can add a type annotation to be explicit, but
   the type annotation is not necessary. *)
let (pi : float) = 3.14 ;;

(* here we overshadowed the previous variable binding pi, with
   another binding also called pi which happens to have the same
   value
*)

(* local bindings :
   the variable m,n,k only exist within the scope of the expression.

   in particular, after evaluating the expression, garbage collection
   will dispose of these variable bindings;

   since they are local bindings, they do not exist globally and cannot
   be accessed by anyone else.
*)

let (m:int) = 3 in
let (n:int) = m * m in  (* n = 9 *)
let (k:int) = m * m in  (* k = 9 *)
 k*n                    (* result will be 81 *)
;;

(* Multiple local bindings which do not depend on each other can
   also be declared using the keyword and. *)
let (m:int) = 3 and
    (n:int) = 4 in
let (k:int) = m + n in  (* k = 7 *)
 k*n                  (* result will be 28 *)
;;


(* Bindings persist -- however later
  binders may overshadow earlier binders.
*)

let (k : int) = 4;;

let (k : int) = 3 in  k * k ;;       (* the result will be 9! *)


k;;                              (* at this point k = 4 again *)

(* Functions *)
(* We can also bind variable to functions. *)
let area : float -> float =
  fun r -> pi *. r *. r;;

(* or more conveniently, we write usually *)
let area (r:float) = pi *. r *. r;;

(* NOTE: Functions are values. *)

let a2 = area 2.0;;      (* result a2 = 12.56 *)

(* We can shadow the definition of pi. *)
let (pi : float) = 6.0;;      (* now pi = 6.0 *)

(* The area function will remain unchanged. ! *)
let a3 = area 2.0;;       (* result a3 = 12.56 *)

(* We can shadow pi with a more accurate definition
   and redeclare area.
 *)
let pi = 4. *. atan 1.;;

let area (r:float) = pi *. r *. r;;

let a4  = area 2.0;;      (* result a4 =  12.5663706144 *)
                          (* it will use the new pi and the new area def. *)

(* function of two arguments: arguments go one after the other *)
let cylinder r h = area r *. h

let a5 = cylinder 2.0 5.0
(* area of a cylinder with radius 2 and height 5 *)


(* Recursive Functions *)
(* recursive functions are declared using the key word let rec and
   their general structure is ;

   let rec name args = body
*)
(*
       val fact:int->int
       fact(n) = n!
       Invariant: n >= 0
       Effects:   none
*)

let rec fact n =
  if n = 0 then 1
  else n * fact (n-1);;


(*
   Checking the invariant (here: n >= 0) each time around
   the loop is often inefficient.
*)

exception Domain;;

let rec fact n =
  if n < 0 then raise Domain
  else if n = 0 then 1
  else n * fact (n-1);;


(*
   Better to check the invariant for the externally
   visible function only, and not during recursion.
*)

let rec fact n =
  let rec f n =
    if n = 0 then
      1
    else
      n * f (n-1)
  in
    if n < 0 then raise Domain
    else f n
;;


(* Tail-recursive version of factorial *)
(*
A function is said to be "tail-recursive", if
there is nothing to do except return the final value.
Since the execution of the function is done, saving
its stack frame (i.e. where we remember the work we still
in general need to do), is redundant.

Instead of nesting the stack deeper, the current stack frame
is reused. The caller is replaced with the callee.
–> Write recursive definitions without worrying about
   space efficiency.

NOTE: All recursive functions can be translated into
  a tail-recursive form!
*)
let rec fact_tr1 n =
  let rec f n m =
    match n with  (* NOTE: Use of pattern matching *)
    | 0 -> m
    | n -> f (n-1) (n*m)
  in
  f n 1
;;

let rec fact_tr1 n =
  let rec f n m =
    if n = 0 then
      m
    else
      f (n-1) (n*m)
  in
  f n 1
;;


(* How do we know they will compute the same result?

   Can we prove that fact_tr2(n) = fact(n) ?

*)


(* More recursive functions... *)

(* add: int -> int -> int *)
let add x y = x + y

(* sum: int -> int *)
let rec sum n =
  if n = 0 then 0 else n + sum (n-1)

(* power n k = n^k for k >= 0 where 0^0 = 1 *)
(* power : int -> int -> int *)
let rec power n k =
  if k = 0 then 1 else n * power n (k-1)

(************************************************************************)
