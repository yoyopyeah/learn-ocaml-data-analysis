(* Lecture 12 : References *)

let r = ref 0
let s = ref 0
let a = r=s
let a' = r==s
let _ = r := 3
let x = !s + !r
let t = r
let b = s=t
let c = r=t
let _ = t := 5
let y = !s + !r
let z = !t + !r


let test () = 
  let pi   = 3.14 in                              (* 1 *)
  let area = (fun r -> pi *. r *. r) in           (* 2 *)
  let a2   = area (2.0) in                        (* 3 *)
  let pi   = 6.0  in                              (* 4 *)
  let a3 = area 2.0 in
  print_string ("Area a2 = " ^ string_of_float a2 ^ "\n");
  print_string ("Area a3 = " ^ string_of_float a3 ^ "\n")
;;



let test_update () = 
  let pi   = ref 3.14 in                              (* 1 *)
  let area = (fun r -> !pi *. r *. r) in              (* 2 *)
  let a2   = area (2.0) in                            (* 3 *)
  let _    = (pi := 6.0) in                           (* 4 *)
  let a3 = area 2.0 in
    (print_string ("Area a2 = " ^ string_of_float a2 ^ "\n");
     print_string ("Area a3 = " ^ string_of_float a3 ^ "\n"))
;;


(* Rotating values - 3 different versions *)
let rot3a (a, b, c) =
    let t = !a in
    (a := !b; b := !c; c := t)
;;

let rot3b (a, b, c) =
  let (x, y, z) = (!a, !b, !c)  in
  (a := y; b := z; c := x)
;;

(* ------------------------------------------- *)


(* Imperative programming using references *)

let imperative_fact n =
  let result = ref 1 in
  for i = 1 to n do
    result := !result * i
  done;
  !result
;;


(* Name generation using a global variable *)
(*  counter to guarantee new variables *)
let counter = ref 0

(* newName () ===> a,  where a is a new name *)
(* A names is a string denoting natural numbers.*)
let newName () =
  counter := !counter + 1;
  "a" ^ string_of_int (!counter)

(* However, this counter is global, and any other function could
   modify it! *)


(* ------------------------------------------------ *)

(* Mimicking object-oriented programming, with _private_ state *)
let (tick, reset) =
  let counter = ref 0 in 
  let tick () =
    incr counter;
    !counter
  in
  let reset () =
    counter := 0
  in 
  (tick , reset)

(* This declaration introduces two functions tick:unit -> int
  and reset:unit -> unit. Their definitions share a private
    variable `counter` that is bound to a reference cell
    containing the current value of a shared counter. The tick
    operation increments the counter and returns its new value, and
    the reset operation resets its value to zero. The types
    already suggest that (implicit) state is involved. 
*)

(** We can mix higher-order functions and effects!
    make_name_generator t p : unit -> string
    and computes a function that each time it's called, generates a
    new name with the prefix `p` and a number generated by calling
    `t`.
 *)
let make_name_generator tick prefix () =
  let n = tick () in
  prefix ^ string_of_int n

let j_gen = make_name_generator tick "j"
let b_gen = make_name_generator tick "b"

(* Note that j_gen and b_gen share the same tick function, so they
   count up together!
   e.g.
   j_gen () ==> "j1"
   b_gen () ==> "b2"
   j_gen () ==> "j3"
   j_gen () ==> "j4"
 *)


(* Suppose we wish to have several different instances of a counter 
   and different pairs of functions tick and reset. We can achieve
   this by defining a counter generator 
*)

(** A record representing the operations of a counter. *)
type counter =
  { tick : unit -> int
  ; reset: unit -> unit
  }

(* newCounter: unit -> {tick : unit -> int, reset: (unit -> unit)} *)
let newCounter () = 
  let counter = ref 0 in 
  { tick = (fun () -> counter := !counter + 1; !counter)
  ; reset = fun () -> counter := 0
  }

(* We've packaged the two operations into a record containing
   two functions that share private state. There is an obvious
   analogy with class-based object-oriented programming. The function
   newCounter may be thought of as a constructor for a
   class of counter objects. Each object has a private instance
   variable counter that is shared between the methods
   tick and reset.

   Here is how to use counters.
*)

let c1 = newCounter()
let c2 = newCounter()

(* Notice, that c1 and c2 are distinct counters! *)

(* c1.tick () ===> 1
   c2.tick () ===> 1
   c1.tick () ===> 2
   so they do not count up together!
 *)

(* Mutable Data structures

So far we have only considered immutable data structures such as lists
or trees, i.e. data structures that it is impossible to change the
structure of the list without building a modified copy of that
structure. Immutable data structures are persistent, i.e. operations
performed on them does not destroy the original structure. This
often makes our implementations easier to understand and reason
about. However, sometimes we do not want to rebuild our data
structure. A classic example is maintaining a dictionary. It is
clearly wasteful if we would need to carry around a large dictionary
and when we want to update it, we need to make a copy of it. This is
What we would like in this case is an "in place update"
operation. For this we must have _ephemeral_ (opposite of
persistent) datastructures. We can achieve this by using references
in OCaml

Consider possibly circular lists. 

*)


(* Code for Reference Lists. *)

type 'a rlist = Empty | RCons of 'a * 'a rlist ref

(* Sometimes you want the tail as a reference, then use tail; 
sometimes you want the actual value, then use cdr .*)

let tail = function
  | Empty -> failwith "empty tail impossible"
  | RCons (_, rs) -> rs

let cdr l = !(tail l)

(* Why `cdr`?
   https://en.wikipedia.org/wiki/CAR_and_CDR
 *)

let l1 = ref (RCons(4, ref Empty))
let l2 = ref (RCons(5, l1))

(* this will create a circular list *)
let _ =  l1 := !l2

(* Observe its output behavior 
   given a reference list l and a bound n
   observe(l,n) will print the first n elements.
 
   If we would not have this bound n, then
   observe would print repeatedly the elements in
   a circular list.
*) 
(* observe: 'a rlist * int -> unit *)
let rec observe l n = begin match l with
  | Empty ->  print_string "0"  
  | RCons(x, l) -> 
      if n = 0 then print_string "STOP\n"
      else (print_string (string_of_int x ^ " "); 
        observe !l (n-1)) 
end


(* rapp : 'a refList * 'a refList -> unit *)
let rec rapp r1 r2 = begin match !r1 with 
  | Empty ->  r1 := !r2
  | RCons (h,t) -> rapp t r2
end


(* This is a destructive reverse function.  *)
(* reverse : 'a refList -> ' a refList *)
let rev l0 = 
  let r = ref Empty in
  let rec rev' l = match !l with
    | Empty -> l0 := !r
    | RCons (h,t) -> 
      (r := RCons(h, ref (!r));
       rev' t)
  in
    (rev' l0; l0)
;;


