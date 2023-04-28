(* -----------------------------------------------------------------*)
(* Exceptions 

Author: Brigitte Pientka, Jacob Errington
Course: COMP 302, McGill University, Montreal, Canada
Copyright © 2019 Brigitte Pientka, Jacob Errington

-------------------------------------------------------------------
Instructor generated course materials (e.g., handouts, notes, summaries,
homeworks, exam questions, etc.) are protected by law and may not be copied
or distributed in any form or in any medium without explicit permission of
the instructor. Note that infringements of copyright can be subject to follow
up by the University under the Code of Student Conduct and Disciplinary
Procedures. 

-------------------------------------------------------------------

OCaml as any other flavor of the ML-language family is a safe language. 
This is ensured by static type checking and by
dynamic checks that rule out violations that cannot be detected
 statically. 

Examples: division by zero, arithmetic overflow

Static violations are signalled by type checking error.
Dynamic violations are signalled by raising an exception.

For example: 

3 / 0 will type-check but cannot be evaluated and will incur a
 runtime fault that is signalled by raising the exception Division_by_zero

An exception is a form of answer to the query: what is the value of
 this expression?

OCaml will report this error as follows:

# 3 / 0;;
Exception: Division_by_zero.
# 

Another source of dynamic run-time error is due to non-exhaustive
 matches. For example:

let head (x::t) = x ;;
Characters 9-19:
  let head (x::t) = x ;;
           ^^^^^^^^^^
Warning P: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val head : 'a list -> 'a = <fun>
# head [];;
Exception: Match_failure ("", 650, -489).
#  

A related situation arises when you pattern match against a specific
pattern in a let or case construct.

# let test l = let h:: _ = l in h;;
Characters 17-22:
  let test l = let h:: _ = l in h;;
                   ^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val test : 'a list -> 'a = <fun>
# test [];;
Exception: Match_failure ("//toplevel//", 286, -7548).

So far we have considered examples of pre-defined exceptions.Since
built-in exceptions have a built-in meaning, we can in fact introduce
new excpetions to signal program specific errors.

*)

(* -----------------------------------------------------------------*)
(* We have briefly seen the use of user-defined exceptions already...
   Here is the example we have seen earlier *)

exception Domain

(*
   Check the invariant for the externally
   visible function only, and not during recursion.
*)

let fact n =
  let rec f n = 
    if n = 0 then 1
    else n * f (n-1)  
  in
  if n < 0 then raise Domain
  else f n

(* Define a wrapper for `fact` that catches the error and nicely prints it. *)
let runFact n =
  try
    let r = fact n in 
    print_string ("Factorial of " ^  string_of_int n ^ " is " ^ string_of_int r ^ "\n")
  with
  | Domain ->
     print_string "Error: Invariant violated -- trying to call factorial on inputs < 0"

(*
# runFact ();;
Calculate factorial of: 3
Factorial of 3 is 6
- : unit = ()

# runFact (-3);;
Error: Invariant violated -- trying to call factorial on inputs < 0 
- : unit = ()
# 
*)

(* -----------------------------------------------------------------*)

type key = int
type 'a btree = 
  | Empty 
  | Node of 'a btree * (key * 'a) * 'a btree

(* Previous solution to checking whether there is an element with key
   k in the tree provided based on option type; 

   findOpt : 'a btree -> int -> 'a opt *)
let rec findOpt t k = match t with 
  | Empty -> None
  | Node(l, (k', _), _) when k < k' -> findOpt l k
  | Node(_, (k', _), r) when k > k' -> findOpt r k
  | Node(_, (_ , x), _) -> Some x

(* Using exceptions to signal error *)
(* New implementation using exception *)

(* Define a program-specific exception with a name *)
exception NoSuchKey of key (* exception includes the problematic key *)

(* Technically, an `exception` declaration is _adding a new
   constructor_ to the preexisting type `exn`.
   When we define such new exception, we can also bundle other data,
   similar to ordinary constructors.
   Then, the try-with construct allows us to catch raised exceptions
   and perform _pattern-matching_ on the `exn` type.
 *)

(* Find (T,k) = d if (k,d) is in the tree T
   raises exception NoSuchKey otherwise *)
let rec find t k = match t with 
  | Empty -> raise (NoSuchKey k) (* raise instead of using None *)
  | Node (l, (k', d), r) when k < k' -> find l k
  | Node (_, (k', _), r) when k > k' -> find r k
  | Node (_, (_,  x), _) -> x (* no need to use Some now *)

(* Find t k n = d 
   searches the tree T up to depth n. If we did no find an entry with key k
   we raise the exception Error with error msg "No entry found in tree"; 
   if we exceeded our bound n, we raise the exception Error with the
   error msg "Bound exceeded" .
*)

exception ErrorMsg of string

let rec findEx t k n = 
  match t with
  | _ when n < 0 -> raise (ErrorMsg "Bound exceeded")
  | Empty ->
     raise (ErrorMsg ("No entry with key " ^ 
			                  string_of_int k ^ 
			                    " found in tree"))
  | Node (l, (k', _), _) when k < k' -> findEx l k (n - 1)
  | Node (_, (k', _), r) when k > k' -> findEx r k (n - 1)
  | Node (_, (_,  x), _) -> x

(* Instead of using strings, we can define separate exceptions.
   This makes it possible for the caller to easily determine which
   error occurred by using try-with.
 *)

exception BoundExceeded
(* We already defined NoSuchKey *)

let rec findEx' t k n = 
  match t with
  | _ when n < 0 -> raise BoundExceeded
  | Empty -> raise (NoSuchKey k)
  | Node (l, (k', _), _) when k < k' -> findEx' l k (n - 1)
  | Node (_, (k', _), r) when k > k' -> findEx' r k (n - 1)
  | Node (_, (_,  x), _) -> x

(***** _Structured_ errors *****
 Idea:
  1. define a datatype to represent a collection of app-specific errors
  2. define a single exception that bundles a value of that type.
  
  Now it is easy to handle every app-specific error that a particular
  function could raise, if we document it to say "it could raise
  LookupError".
 *)

type lookup_error =
  | BoundExceeded
  | NoSuchKey of key

exception LookupError of lookup_error 

(* Define a helper for raising lookup errors. *)
let lookup_error e = raise (LookupError e)

let rec findEx'' t k n = 
  match t with
  | _ when n < 0 -> lookup_error BoundExceeded
  | Empty -> lookup_error (NoSuchKey k)
  | Node (l, (k', _), _) when k < k' -> findEx' l k (n - 1)
  | Node (_, (k', _), r) when k > k' -> findEx' r k (n - 1)
  | Node (_, (_,  x), _) -> x


(* -----------------------------------------------------------------*)

(* Some top level functions which illustrate how to
 handle exceptions; we contrast it to how to handle
 optional value *)

let findOpt_top t k = 
  match findOpt t k with
  | None   -> print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")
  | Some d -> print_string ("Found element " ^ string_of_int d ^ " in tree \n")

(* -----------------------------------------------------------------*)
(* HANDLING OF EXCEPTIONS 
 
   try exp1 with exc_pat -> exp 2

   installs an exception handler.

   This allows for non-local transfer of control. We may catch a
   raised exception and continue evaluating along some other path.

   OCaml attempts to evaluate the expression exp1. If this yields a value, then this
   value is returned. If evaluation of part of expression exp1 raises an exception exc
   then evalution of exp1 will stop possibly midway and exc is matched against exc_pat
   to determine how to proceed.  

   if there is no handler or no successful match, the exception
   will re-raised and propagated to the next higher level.

   If it is never caught at any higher level, the computation will
   abort with an uncaugt exception exc

*)

let find_top t k = 
  try
    let d = find t k in  
    (* the exception raised by find(T,k) will propel you 
	     to the next higher level! -- nothing below this line
	     in particular the statement print "Found element ..."
	     will not be executed *)
    print_string ("Found element " ^ string_of_int d ^ " in tree \n")
  with NoSuchKey k ->
    print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")


(* we can pattern match on exceptions under `with`. *)
let find_toplevel t k = 
 try
   findEx t k 4
 with 
 | ErrorMsg msg ->  print_string msg

(* we can also dispatch on the different exceptions *)
let find_top' fmt t k = 
  (* exercise: what is the type of fmt? *)
 try
   let x = findEx' t k 4 in
   print_string ("Found element: " ^ fmt x)
 with 
 | BoundExceeded ->  print_string ("Bound exceeded  \n")
 | NoSuchKey k ->  print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")
     
(* -----------------------------------------------------------------*)
(* Exceptions are propogated to the next level, if they are not caught! *)
let find_uncaught t k = find t k

let find_catch t k = 
  try 
    let d = find_uncaught t k in 
    print_string ("Found element " ^ string_of_int d ^ " in tree \n")
  with
  | NoSuchKey k -> print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")

(* We can define an exception-safe wrapper for findEx'' leveraging
   higher-order functions to handle only the app-specific errors. *)
let safe_findEx'' handler t k n  =
  (* Install an exception handler for LookupError using the try-with syntax.
     More on this below.
   *)
  try findEx'' t k n with
  | LookupError e -> handler e
(* any other exception will simply be reraised *)

(* e.g. handled_findEx'' performs the search, and handles only the
  app-specific errors by nicely printing them and cleanly exiting the
  program.
 *)
let handled_findEx'' t k n =
  safe_findEx''
    (function
     | BoundExceeded ->
        print_string ("Search bound " ^ string_of_int n ^ " exceeded");
        exit 1
     | NoSuchKey k ->
        print_string ("Key " ^ string_of_int k ^ " not found.");
        exit 1)
    t k n

(* Structured errors give an excellent way to separate the concerns of the program:
   1. the searching is unobscured by exception handling logic
   2. the handler logic is parameterized via a higher-order function,
      so different handling logic can be plugged in as needed.
   3. by partially applying the handler function, we recover a safe
      lookup function that performs the already specified error-recovery
      logic.
 *)

(* The primary benefit of exceptions:
   Separating the happy path (e.g. tree searching) from error
   handling.
*)

(* This use of exceptions to signal error conditions sugests raising
   exceptions is fatal: an unhandled exception terminates the program
   -- but signalling an error is only one use of exceptions! 
   We can instead use exceptions to construct interesting types of
   control flow.
*)


(* -----------------------------------------------------------------*)
(* USING EXCEPTIONS FOR BACKTRACKING - Diverting the control flow                    *)
(* Use of exceptions to initiate backtracking in a tree
   which is not necessarily a binary search tree *)
exception NotFound
let rec find_gen t k = match t with
  | Empty -> raise NotFound
  | Node (_, (k',d), _) when k = k' -> d
  | Node (l, _, r) ->
	   try
       find_gen l k
     with
     | NotFound ->
        find_gen r k

(* Exercise: reimplement `find_gen` to use option instead of
   exceptions for errors. *)

(* -----------------------------------------------------------------*)
(*
Consider what happens when we evaluate and understand how
exception handlers are installed.

l = Node ( Node (Empty, (3, "3"), Empty)  , (7, "7"), Node (Empty, (44, "44"), Empty)), 
r = Node ( Node (Empty, (55, "55"), Empty)  , (8, "8"), Empty
t = Node(
      l      
      (1, "1"), 
      r) 

find_gen t 55
==> try find_gen l 55 
    with NotFound -> find_gen r 55
==> try 
       (try find_gen (Node (Empty, (3, "3"), Empty) )
        with NotFound -> find_gen (Node (Empty, (44, "44"), Empty) ))
    with NotFound -> find_gen r 55

==> try 
       (try 
          (try find_gen  Empty with NotFound -> Empty)
        with NotFound -> find_gen (Node (Empty, (44, "44"), Empty) ))
    with NotFound -> find_gen r 55

==> try  find_gen (Node (Empty, (44, "44"), Empty) )
      with NotFound -> find_gen r 55

...
  

*)


(* -----------------------------------------------------------------*)
(* Making change with given coins
   [5;2] means we have an infinite number of 5 and 2 coins.

# change [50;25;10;5;2;1] 43;;
- : int list = [25; 10; 5; 2; 1]
# change [50;25;10;5;2;1] 13;;
- : int list = [10; 2; 1]
# change [5;2;1] 13;;
- : int list = [5; 5; 2; 1]
# 

The idea is to proceed greedily, but if we get stuck,
we undo the most recent greedy decision and proceed again from there.

*)
(* First some toString conversion function  as a helper function 
   this function is used for printing later on.
   Exercise: generalize this to a higher-order function of type:
   string_of_list : ('a -> string) -> 'a list -> string
   such that for any list l : int list,
   string_of_list string_of_int l = listToString l
*)
let rec listToString l = match l with 
  | [] -> ""
  | [x] -> string_of_int x
  | x :: xs -> string_of_int x ^ ", " ^ listToString xs

(* Define Exception we'll use to manipulate the control flow *)
exception Change

(* change : int list -> int -> int list *)
let rec change coins amt = 
  match coins with
  (* If the amount of change to make is zero, then we're done. *)
  | _ when amt = 0 -> []
  | [] ->
     (* if we run out of available coins but amt is nonzero, then we
        fail with an exception to jump back to the nearest handler. *)
     raise Change
  | coin :: cs when coin > amt ->
     (* if this coin is too large, we forget about it for now and
     consider the other coins *)
     change cs amt
  | coin :: cs ->
	   try 
       (* otherwise, we know this coin is good, so we add it to the
          return list and recursively make change for the remaining sum.
        *)
	     coin :: change coins (amt - coin)
	   with
     | Change ->
        (* If it turns out that by using `coin` we were unable to make
           the remaining change, then we forget about `coin` and try to
           make change for the full amount with the remaining coins.
         *)
        change cs amt

(* EXERCISE: reimplement `change` using option instead of exceptions. *)

let change_top coins amt =
  try 
    let c = change coins amt in
    print_string ("Return the following change: " ^ listToString c ^ "\n")
  with Change ->
    print_string ("Sorry, I cannot give change\n")

(* Here is the behavior of change_top : 

# change_top [2;5] 3;;
Sorry, I cannot give change
- : unit = ()
# change_top [2;5] 8;;
Return the following change: 2, 2, 2, 2
- : unit = ()
# change_top [25;10;5;2] 43;;
Return the following change: 25, 10, 2, 2, 2, 2
- : unit = ()
# change_top [25;10;5;2] 44;;
Return the following change: 25, 10, 5, 2, 2
- : unit = ()
# change_top [25;10;2] 44;;
Return the following change: 10, 10, 10, 10, 2, 2
- : unit = ()
# change_top [25;10;2] 43;;
Return the following change: 25, 10, 2, 2, 2, 2
- : unit = ()
# change_top [25;10;2] 23;;
Sorry, I cannot give change
- : unit = ()
# 
*)
