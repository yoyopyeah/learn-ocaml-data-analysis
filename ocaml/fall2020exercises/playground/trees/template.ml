(************************************************************************)
(* Datatypes: Binary Trees                                              *)
(* Binary trees are one of the most common data-structures used.
 
   Inductive definition of binary trees:

   o The empty binary tree "Empty" is a binary tree.
   o If l and r are binary trees and v is a value of type 'a
     then Node (l, v, r) is a binary tree.
   o Nothing else is a binary tree.

*)
(************************************************************************)
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

(* size of a tree
   size: 'a tree -> int

   size(T) = n where n is size of the tree determined by
   the number of nodes. 
*)
let rec size t = match t with 
| Empty          -> 0
| Node (l, _, r) -> size l + size r + 1


(* Food-for thought question: 

   1) How could we define an n-ary tree? 
   
   2) How could one define a red-black tree? 
 
 *)

(* insert:  'a * 'b -> ('a * 'b) tree -> ('a * 'b) tree
   
   insert (x,d) T = T'  where (x,d) has been inserted into T
   and any previous occurrences of (x,d') in T have been
   overwritten
  
*)

let rec insert e t = match t with 
  | Empty -> Node (Empty, e, Empty)
  | Node (l, e', r) ->
    if fst e = fst e'
    then Node (l, e, r)
    else if fst e < fst e'
         then Node (insert e l, e', r)
         else Node (l, e', insert e r)

(* NOTE: Although the type of the function insert is polymorphic
   in 'a and 'b, it depends on the fact that we do have  functions 
   to compare elements of type 'a!  
*)

(* Remark about comparison operators. 

    = and < are meaningful for numbers, strings, and characters. 
    BUT in OCaml you can also compare any other object; this is 
    to be used cautiously, since it may or may not  reflect your
    intended meaning of less or equal. 

    For example: The following looks entirely reasonable and 
      is accepted by OCaml.

# Empty < Node (3, Empty, Empty);;
- : bool = true
# Node (3, Empty, Empty) < Node (4, Empty, Empty);;
- : bool = true
# Node (3, Empty, Empty) < Node (2, Empty, Empty);;
- : bool = false

    However, the following is also accepted.

# [3;4] < [7];;
- : bool = true
# [3;4] < [2];;
- : bool = false

# [3;3;4] < [6;7;4];;
- : bool = true
#  [3;3;4] < [1;7;4];;
- : bool = false

   Clearly, OCaml compares the lists by comparing their elements; 
   but it does not take into account the length of the list. 

   For better, more readable and easier to understand code, we recommend 
   you write out equality definition explicitely. In fact, some languages
   do not allow you to compare data constructed from constructors via 
   built-in operators such as = or <.

*)

(* lookup :  'a -> ('a * 'b) tree -> 'b option
   
   lookup x T = 
   
   if there exists a node in T with key x and data d,
   then return Some(d) else None

   NOTE: Although the type of this function is polymorphic
   in 'a and 'b, it depends on the fact that we do have
   functions to compare elements of type 'a!

*)

let rec lookup k t = match t with 
  | Empty -> None
  | Node (l, (k', x), r) -> 
    if k = k' then
      Some x
    else
      if k < k' then lookup k l else lookup k r

(* NOTE: We enforce the following property : 
    lookup x (insert (x,d) t) = Some (d)

Case t = Empty

    lookup x (insert (x,dx) Empty) 
==> lookup x (Node ( (x,dx), Empty, Empty) 
==> Some dx

Case t = Node ( (y,dy), l, r)
IH 1: lookup x (insert (x,dx) l) ==> Some dx
IH 2: lookup x (insert (x,dx) r) ==> Some dx

TO SHOW: lookup x (insert (x, dx) (Node ( (y, dy), l , r))) ==> Some dx

x = y : 
    lookup x (insert (x, dx) (Node ( (y, dy), l , r)))
==> lookup x (Node ( (x, dx) , l, r) ==> Some dx

x < y : 
    lookup x (insert (x, dx) (Node ( (y, dy), l , r)))
==> lookup x (Node ( (y, dy), insert (x, dx) l , r))
==> lookup x (insert (x,dx) l) ==IH==> Some dx

x > y : 
    lookup x (insert (x, dx) (Node ( (y, dy), l , r)))
==> lookup x (Node ( (y, dy), l, insert (x, dx) r))
==> lookup x (insert (x,dx) r) ==IH==> Some dx

*)

(************************************************************************)
(* Examples of trees *)

let leaf x = Node (Empty, x, Empty)
let t1 =
  Node
    ( Node
        ( leaf 2
        , 3
        , Node
            ( leaf 4
            , 5
            , leaf 6
            )
        )
    , 9
    , Node
        ( leaf 11
        , 13
        , leaf 15
        )
    );;


(*             9
            /     \
           3       13
         /  \     /  \ 
        2    5   11   15
            / \
           4   6
*)
