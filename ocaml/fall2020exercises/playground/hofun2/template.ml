(* Higher-order functions -- Part 2 
   Returning functions as results *)

(* curry : (('a * 'b) -> 'c) -> ('a -> 'b -> 'c) *)
(* this is equivalent to (('a * 'b) -> 'c) -> 'a -> 'b -> 'c 

   Note : Arrows are right-associative. *)
let curry f = (fun x y -> f (x,y))

(* Also note: the following function will have the
   same type as curry. 

   When we only give curry' a concrete function test: 'a * 'b -> 'c
   then curry' will be partially evaluated, and produce a 
   function (fun x -> fun y -> f (x,y) which is equivalent to 
   (fun x y -> f (x,y)).

   Remember: fun describes an anonymous function which can take in
     multiple arguments, but does not allow pattern matching on the
     arguments.
*)   
let curry' f x y = f (x,y) 

(* A third way of writing which emphasizes that functions
   are values and taking in 3 values. *)

let cur = fun f -> fun x -> fun y -> f (x,y)


(* What happens if we try and execute the following:

# let plus (x,y) = x + y;;
val plus : int * int -> int = <fun>
# cur plus ;;
- : int -> int -> int = <fun>
# curry plus ;;
- : int -> int -> int = <fun>
# 

Why can we not *see* the return function?
Meaning why doesn't it show the actual function?

well, functions are values and they evaluate to themselves.
ML-like language like OCaml do not print back to you the "code" for
the function plus, it will not print back to you the code
for the function we get by  (curry plus)

To understand what is happening, we really need to
have an understanding of the operational semantics...   
*) 



(* uncurry: ('a -> 'b -> 'c) -> (('a * 'b) -> 'c) *)
let uncurry f = (fun (y,x) -> f y x)

(* 

# let uncurry f = (fun (y,x) -> f y x);;
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun>

# uncurry (curry plus);;
- : int * int -> int = <fun>

Can one prove:

For any function f, uncurry (curry f) = f? 

- Debugger not very helpful.

#trace curry;;
curry is now traced.
# #trace uncurry;;
uncurry is now traced.
# uncurry (curry plus);;
curry <-- <fun>
curry --> <fun>
uncurry <-- <fun>
uncurry --> <fun>
- : int * int -> int = <fun>

 - Not so easy...because what we get is not directly f
   (technically...) 
+—————————————————————————–+
| we need to have an understanding  |
| of the underlying semantics.      |
+—————————————————————————–+

uncurry (curry f) ==>
uncurry (fun x -> fun y -> f (x,y)) ==>
fun (y,x) -> ((fun x -> fun y -> f (x,y)) x y) ==>

BUT for every input x and y,

 (uncurry (curry f)) x y = f (x,y) !


*)

(* swap : ('a * 'b -> 'c) -> ('b * 'a -> 'c) *)
let swap f = (fun (x,y) -> f (y,x))


(* ---------------------------------------------------------- *)
(* derivative : x^17 = 17 * x^16 

   df/dx = lim(e -> 0)   (f(x+e) - f(x))/e 


   approximately computing derivatives:

df/dx = (f(x + e) - f(x))/e  where e is small
deriv: (float -> float) * float -> (float -> float) 
*)

let deriv (f, dx) = fun x -> (f (x +. dx) -. f x) /. dx


(* ---------------------------------------------------------- *)

(* halfInterval (f, (a,b) , t) = (a', b') 

Given a function f : float -> float,
      two float numbers a, b s.t. f(a) < 0 and f(b) > 0
  and a float number tolerance t,

 the x for which f(x) = 0 will be between  a' and b',
 and the size of this interval is t.

val halfInterval: (float -> float) -> (float * float) -> float -> (float * float)
*)

let abs_float x = if (x < 0.0) then -.x else x 

let rec halfint f (a, b) t =
  let mid = (a +. b) /. 2.0 in
    if  abs_float (f mid) < t
      then mid
    else
      if (f mid) < 0.0
        then halfint f (mid, b) t
      else halfint f (a, mid) t

(* -------------------------------------------------------------------- *)
(* OCAML HACKING with higher-order functions  
  
   Enabling generic "infix notation" aka writing expression as infix   *)

  let ( <| ) x y = y x
   and ( |> ) x y = x y

(*
val ( <| ) : 'a -> ('a -> 'b) -> 'b 
val ( |> ) : ('a -> 'b) -> 'a -> 'b
*)

(*  NOTE:  OCAml has a predefined set of infix symbols

infix-symbol    ::= (= ∣  < ∣  > ∣  @ ∣  ^ ∣  | ∣  & ∣  + ∣  - ∣  * ∣  / ∣  $ ∣  %) { operator-char }  
operator-char   ::= ! ∣  $ ∣  % ∣  & ∣  * ∣  + ∣  - ∣  . ∣  / ∣  : ∣  < ∣  = ∣  > ∣  ? ∣  @ ∣  ^ ∣  | ∣  ~  

See http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#operator-char

We are using here the fact that <| is a valid infix symbol according to the
OCaml grammar. We could have similarly used  =|  or @ or */ etc.

 *)

(* Example of using our new infix operator *)
  let r  = 3 <| (fun x y -> x < y) |> 4 


(* NOTE: the previous line is in fact equivalent to the following  *)
  let r' = 3 <| ( (fun x y -> x < y) |> 4  )


(* -------------------------------------------------------------------- *)
(* Partial evaluation, simple staged computation example,
   demonstrating efficiency gain:    *)

(* plus : int -> int -> int 
   plusSq = fun x -> fun y -> x + y*y *)

let plusSq x y =  x * x + y * y


(* plus3 : int -> int *)
let plus3 = (plusSq 3)

(* Note the resulting function plus3 (y) = 3 * 3 + y * y   

   SML will not evaluate 3*3 to 9! and produce 

   plus3(y) = 9 + y * y


   The function closure shields the function body, and
   the body can only be evaluated when we pass an argument
   for y. 
         
 *)


(* val horriblecomputation : int -> int *)
let rec horriblecomputation x =
  let rec ackermann m n = match (m , n) with 
    | 0 , n ->  n+1
    | m , 0 -> ackermann (m-1) 1
    | m , n -> ackermann (m-1) (ackermann m (n-1)) in
  let y = (abs x) mod 3 + 2 in
  let rec count n = match n with 
    | 0 -> ackermann y 4
    | n -> count(n-1) + 0 * ackermann y 4 in
  let large = 1000 in 
    (ackermann y 1) * (ackermann y 2) * (ackermann y 3) * count large
    


(* Unstaged uncurried version: *)
(* val f1 : int * int -> int     *)
let f1 (x, y) =
  let z = horriblecomputation x in
    z + y


(* The horrible computation is performed each time: *)
let result1 = f1(10, 5)
let result2 = f1(10, 2)
let result3 = f1(10, 18)


(* Unstaged curried version:  *)
(* val f2 : int -> int -> int *)
let f2 x y =
  let z = horriblecomputation x in
    z + y

(* val f2' : int -> int *)
let f2' = f2 10

(* NOTE : 10 is only substituted into the body for x
   yielding

  fun y -> 
  let z = horriblecomputation 10 in z + y

  The closue / function shields its evaluation!

*)



(* Hence, the horrible computation is again performed each time: *)
let result1 = f2' 5
let result2 = f2' 2
let result3 = f2' 18

(* Staged curried version: 
   Note how we now create the function (fn y -> ...)
   *after* the horrible computation.
*)

(* let f3 : int -> int -> int *)

let f3 x =
  let z = horriblecomputation x in      (* HERE COMPUTATION HAPPENS *)
    (fun y -> z + y)                (*  ITS RESULT WILL BE SUBSTITUTED IN THE BODY 
                      OF THE LET-EXPRESSION ! *) 



(* The horrible computation is performed once, 
   during the declaration of f3': 
*)
(* f3' : int -> int *)
let f3' = f3 10

let res1 = f3' 5
let res2 = f3' 2
let res3 = f3' 18


(* ------------------------------------------------------------------- *)
(* Code generation:                                                    *)
(* Previously in our discussion about higher-order functions
   we also briefly touched on partial evaluation and code generation.
   Note, that in code generation we are also essentially constructing
   a stack of functions - the main difference is that when we are 
   generating code our acc is meant to be independent of the function
   we are currently defining.
*)
(* ------------------------------------------------------------------- *)
(* pow k n = n^k *)
let rec pow k n = if k = 0 then 1 
  else n * pow (k-1) n
                         
(* Tail-recursive *)
let rec pow_tl k n acc = if k = 0 then acc
  else pow_tl (k-1) n (n * acc)

(* (pow_tl 2) => fu n -> fun acc -> pow_tl 1 n (n * acc) *)


(* function generater *)
let rec powGen k cont = if k = 0 then cont
  else powGen (k-1) (fun n -> n * (cont n))


(*   powGen 2 (fun n0 -> 1) 
 ->  powGen 1 (fun n1 -> n1 * (fun n0 -> 1) n1)
 ->  powGen 0 (fun n2 -> n2 * (fun n1 -> n1 * (fun n0 -> 1) n1) n2)
 ->  (fun n2 -> n2 * (fun n1 -> n1 * (fun n0 -> 1) n1) n2)
                     ----------------------------------
    = fun n2 -> n2 *         n2 * (fun n0 -> 1) n2
    = fun n2 -> n2 * n2 * 1

    which is indeed the square function!
*)

