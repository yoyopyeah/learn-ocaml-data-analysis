(* NOTE: Remove this line before posting the solution.
   It is only here so that the grader can find tabulate,
   which was defined in the prelude.
*)
let tabulate = tabulate

(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     For example (this isn't a valid test case, so don't try to use it):
     let f x = "COMP 302" in
     ((f, 2), ["COMP 302"; "COMP 302"; "COMP 302"])
  *)
  (((fun x -> x), 0), [0]);
  (((fun x -> x), 3), [0; 1; 2; 3])
]


(* TODO: Implement dist_table: (int * int) -> int -> float list *)
(* 5 points *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  tabulate (fun n -> dist_black n x (marblesTotal, marblesDrawn)) marblesTotal

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([[]], true);
  ([[1.]], false);
  ([[]; []; []], true)
]

(* 40 points total for coding max_likelihood  *)                                                  
(* TODO: Implement is_empty: 'a list list -> bool *)
(* 5 points *)
let is_empty (matrix: 'a list list) : bool =
  List.for_all (fun l -> l = []) matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
(* 5 points *)  
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map (fun x -> dist_table (total, drawn) x) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
(* 20 points *)  
let rec combined_dist_table (matrix: float list list) =
  if is_empty matrix then []
  else
    let get_heads = List.map (fun (h::t) -> h) matrix  in
    let get_tails = List.map (fun (h::t) -> t) matrix  in
    let result    = List.fold_right (fun x r -> x *. r) get_heads 1.0 in
      result :: combined_dist_table get_tails

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
 *)

let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
      (dist_matrix (total, drawn) resultList))


                                               

(* Q2 Cake Traversals - Total : 50 points*)

(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
(* Q2a 10 points *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  match c with 
  | Slice l ->  p l
  | Cake (c1, c2) -> all p c1 && all p c2



(* Q2b 10 points, 3 for tests, 7 for solution *)
let is_chocolate_cake_tests = [
  ((Slice []), false);
  ((Cake (Slice [Orange], Slice [])), false);
  ((Cake (Slice [], Slice [Almonds; Flour])), false);
  ((Slice [Chocolate]), true);
  ((Cake (Slice [Chocolate], Slice [Chocolate])), true);
  ((Cake (Slice [Orange; Flour], Slice [Chocolate])), false);
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool =                
  all (fun il -> List.exists (fun i -> i = Chocolate) il) c


(* Q2c 10 points*)
(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  match c with 
  | Slice il -> Slice (p il)
  | Cake (c1, c2) -> Cake (map p c1, map p c2)


(* Q2d 5 points, 2 for tests, 3 for solution *)
let add_ingredient_tests = [
  ( (Orange, Slice []), Slice [Orange]);
  ( (Orange, (Cake (Slice[], Slice []))) , (Cake (Slice[Orange], Slice [Orange])) );
  ( (Orange, (Cake (Slice[Chocolate], Slice [Chocolate]))) , (Cake (Slice[Chocolate; Orange], Slice [Chocolate; Orange])) )
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  map (fun il -> insert x il) c                  


(* Q2e 10 points *)
(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  match c with   
  | Slice il -> f il base
  | Cake (c1, c2) -> (fold_cake f (fold_cake f base c1) c2)


(* Q2f 5 points *)
(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  fold_cake (fun il base -> union il base) [] c 

