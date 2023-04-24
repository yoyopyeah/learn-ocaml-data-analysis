(* First some basic fuctions we will use *)
let rec cube n = n*n*n 
let rec rcube n = n *. n *. n 
let rec square n = n * n 
let rec exp (b,n) = if n = 0 then 1 else b * exp(b, n-1)


(* Here are some well-known sums *)
(* sumInts(a,b) = sum k from k=a to k=b  *)
let rec sumInts(a,b) = if (a > b) then 0 else a + sumInts(a+1,b)

(* sumSquare(a,b) = sum k^2 from k=a to k=b  *)
let rec sumSquare(a,b) = if (a > b) then 0 else square(a) + sumSquare(a+1,b)

(* sumCubes(a,b) = sum k^3 from k=a to k=b  *)
let rec sumCubes(a,b) = if (a > b) then 0 else cube(a) + sumCubes(a+1,b)

(* sumExp(a,b) = sum 2^k from k = a to k = b *)
let rec sumExp(a,b) = if (a > b) then 0 else exp(2,a) + sumExp(a+1,b)



(*------------------------------------------------------------------- *)
(* Step 1: 
   We will abstract over the function f (i.e. cube, square, exp etc) 
   to get a general sum function. *)

(* sum: (int -> int) -> int * int -> int *)
let rec sum f (a, b) =
  if (a > b) then 0
  else (f a) + sum f (a+1, b)

(* Now we can get the previous sums as follows *)
let rec sumInts'  (a,b) = sum (fun x -> x) (a, b)
let rec sumCubes' (a,b) = sum cube (a, b) 
let rec sumSquare'(a,b) = sum square (a, b) 
let rec sumExp'   (a,b) = sum (fun x -> exp(2,x)) (a, b) 

(* Side remark regarding the difference of writing anonymous function with

fun and function:

 - function allows the use of pattern matching (i.e. |), but consequently it can be passed only one argument.

    function p_1 -> exp_1 | … | p_n -> exp_n

    is equivalent to

    fun exp -> match exp with p_1 -> exp_1 | … | p_n -> exp_n

 - fun does not allow pattern matching, but can be passed multiple arguments, e.g.

    fun x y -> x + y

When either of the two forms can be used, fun is generally preferred due to its compactness.

*)

(*------------------------------------------------------------------- *)
(* What if we want to abstract over the increment function? *)
(* For example, if we want to sum up all the odd numbers? *)

(* sumOdd: int -> int -> int *)
let rec sumOdd (a, b) = 
  let rec sum' (a,b) = 
      if a > b then 0
      else a + sum'(a+2, b)
  in  
    if (a mod 2) = 1 then
      (* a was odd *)
      sum'(a,b)
    else 
      (* a was even *)
      sum'(a+1, b)

    

(* sum': (int -> int) -> int * int -> (inc -> inc) -> int *)
let rec sum' f (a, b) inc = 
    if (a > b) then 0
    else (f a) + sum' f (inc(a),b) inc

(* tail-recursive version of sum *)
let sum_tr f (a,b) inc = 
  let rec sum (a,b) acc = 
    if a > b then acc 
    else sum (inc a, b) (f(a) + acc)
  in 
    sum (a,b) 0



(* Now we can rewrite sumOdd using sum' *)
let rec sumOdd' (a,b) = 
  if (a mod 2) = 1 then    
    sum' (fun x -> x) (a, b) (fun x -> x + 2)
  else 
    sum' (fun x -> x) (a+1, b) (fun x -> x + 2)

(*------------------------------------------------------------------- *)

(* Product in analogy with sum.  *)
let rec product f (a,b) inc =
  if (a > b) then 1
  else (f a) * product f (inc(a), b) inc

(* tail recursive version of product *)
let prod_tr f (a,b) inc = 
  let rec prod (a,b) acc =
    if (a > b) then acc
    else prod (inc(a), b) (f(a) * acc) 
  in 
    prod (a, b) 1

(* Using product to define factorial. *)
let rec fact n = product (fun x -> x) (1, n) (fun x -> x + 1)



(*------------------------------------------------------------------- *)
(* The general series written in the tail-recursive form. *)
(* series: 
     (int -> int -> int) ->    combiner
     (int -> int) ->           f  
     int * int ->              a and hi
     (int -> int) ->           inc 
     int                       acc
  -> int 

*)
let rec series comb f (a,hi) inc r =
  let rec series' (a, r) = 
      if (a > hi) then r
      else
    series' (inc(a), comb r (f a))
  in 
    series'(a, r)


let rec sumSeries  f (a,b) inc = series (fun x y -> x + y) f (a, b) inc 0
let rec prodSeries f (a,b) inc = series (fun x y -> x * y) f (a, b) inc 1

(*------------------------------------------------------------------- *)
(* An iterative version of sum modified to deal with real values. *)

let rec iter_sum f (lo, hi) inc =
  let rec sum' (lo, r) = 
    if (lo > hi) then r
    else 
      sum' (inc(lo), r +. f lo)
  in 
    sum' (lo, 0.0)



(* The integral of f(x) is the area below the curve f(x) in the interval [a, b]. 
   We will use rectangle method to approximate the integral of f(x) in 
   the interval [a,b], made by summing up a series of small rectangles
   using midpoint approximation.
*)

let rec integral f (lo,hi) dx =
    dx *. iter_sum f (lo +. (dx /. 2.0) , hi) (fun x -> x +. dx) 


let rec integral' f (lo,hi) dx =
    iter_sum (fun x -> f x *. dx) (lo +. dx /. 2.0, hi) (fun x -> x +. dx) 


(*------------------------------------------------------------------- *)
(* Half interval method                                               *)
(*------------------------------------------------------------------- *)
let rec halfint (f:float -> float) (a, b) t =
  let mid = (a +. b) /. 2.0   in
    if  abs_float (f mid) < t then mid
    else
      if (f mid) < 0.0
        then halfint f (mid, b) t
      else halfint f (a, mid) t

(*------------------------------------------------------------------- *)
(* Combining higher order functions with recursive datatypes:         *)
(*------------------------------------------------------------------- *)

(* map : ('a -> 'b) -> 'a list -> 'b list 
   
   map f l = l'  where l' is the list we obtain by applying f to
   each element in l

*)

let rec map f l = match l with 
  | []  -> []
  | h::t -> (f h)::(map f t)

(* filter : ('a -> bool) -> 'a list -> 'a list

   filter p l ==> sublist containing the elements of l for which p holds.

   Invariants: none
   Effects: none
*)

let rec filter (p : 'a -> bool) l = match l with 
  | []   -> []
  | x::l ->  
    if p x then x::filter p l
    else filter p l


(* let pos : int list -> int list
       pos(l) ==> sublist of l consisting of strictly positive integers.

   Invariants: none
   Effects: none
*)

let (pos : int list -> int list) = filter (fun n -> n > 0)

(* Now try: *)

let r = pos [2; -1; -4; 3; 0; 8]

(* fold_right  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
(* fold_right  f [x1; x2; ...; xn] init
    returns

    f x1 (f x2 ... (f xn init)...)

    or init if the list is empty. 
*)
let rec fold_right  f l b = match l with 
  | []   -> b
  | h::t -> f h (fold_right f t b)

(* fold_left f init [x1; x2; ...; xn]
   returns

    f xn...(f x2 (f x1 init))...)

    or init if the list is empty. 

*)
let rec fold_left f b l  = match l with 
  | []   -> b
  | h::t -> fold_left f (f h b) t

(*

# fold_right (fun x b -> string_of_int x ^ b) [1;2;3;4;5] "" ;;
- : string = "12345"

# fold_left (fun x b -> string_of_int x ^ b) "" [1;2;3;4;5]  "" ;;
- : string = "54321"
# 
*)


(* tabulate (n, f)
    returns a list of length n equal to [f(0), f(1), ..., f(n-1)], 
    created from left to right. It raises Size if n < 0.  
 *)

let rec tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n - 1) ((f n) :: acc)
  in
  tab (n - 1) []


        



