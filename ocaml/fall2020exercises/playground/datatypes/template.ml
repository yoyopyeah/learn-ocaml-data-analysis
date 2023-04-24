(* Code for Lecture 3: Datatypes                         *)

(************************************************************************)

(* Datatypes (non-recursive) *)

(* suit of cards *)
type suit = Clubs | Spades | Hearts | Diamonds

(* We can extend the pre-defined types of int, float, char, string, etc.
   with user-defined types.

   Here declare a collection of elements belonging to the type suit; 
   we also say Clubs, Spades, Hearts, and Diamonds inhabit the type suit. 
   Often, we simply say  Clubs (Spades, Hearts, Diamonds) *is* of type suit. 

   Note: 
   - the order in which we declare these elements does not matter
   - Elements (= constructors) Need to begin with a capital letter
   - Taking apart elements of a type is done by pattern matching
     using the expression
 
     match <expression> with  
      | <pattern> -> <expression>
      | <pattern> -> <expression> 
        ...
      | <pattern> -> <expression> 
*)

(* dom : suit*suit -> bool

   dom(s1,s2) = true iff suit s1 beats or is equal to suit s2
                relative to the ordering S > H > D > C         
   Invariants: none
   Effects: none
*)

let rec dom s1 s2 = match (s1, s2) with
  | (Spades, _)        -> true
  | (Hearts, Diamonds) -> true
  | (Hearts, Clubs)    -> true
  | (Diamonds, Clubs)  -> true
  | (s1, s2)           -> s1 = s2

(* 
   Patterns:
   - Allow pattern matching on the possible elements of a type
   - Allows deep pattern matching
   - Wildcard _ matches anything
   _ (s1, s2) is a generic pattern which matches anything, but
     we have names, namely s1 and s2, to refer to the parts of the tuple. 
   - All variables in a pattern must be distinct;
     i.e. (s1, s1) is not a valid pattern.

*)


(* rank of cards *)
type rank = 
 Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
 Jack | Queen | King | Ace

(* A card is a rank and a suit: represent "and" with a tuple *)
type card = rank * suit


(* Recursive Data-type *)
(* Let's define the collection of cards in my hand. We want to define 
   it in such a way, that it can describe the fact that I have 
   2 cards in my hand, 77 cards in my hand, or no card at all in my hand.

   We will define it recursively:

   A hand is either empty or 
   it consists of a card followed by the rest of the hand 

   More precisely, we can define the elements of type hand as follows:
   - Empty is of type hand
   - If c is a card and h is of type hand 
     then Hand (c, h) is of type hand.
   - Nothing else is of type hand.

*)

(* Data-type (recursive!) *)
type hand = Empty | Hand of card * hand

(* Empty and Hand are constructors. They allow us to construct
   an element of type hand. 

*)


(* Some sample hands: *)

let hand0 : hand = Empty
let hand1 : hand = Hand((Ace, Hearts), Empty)
let hand2 : hand = Hand((Queen, Diamonds), hand1)
let hand5 : hand =
  Hand
    ( (Ace, Spades)
    , Hand
        ( (Ten, Diamonds)
        , Hand
            ( (Seven, Clubs)
            , Hand
                ( (Queen, Spades)
                , Hand
                    ( (Eight, Clubs)
                    , Empty
                    )
                )
            )
        )
    )

(* extract : suit -> hand -> hand
   extract s h returns a hand consisting of all card in h of suit s.
   Invariants: none
   Effects: none
*)

let rec extract (s : suit) (h : hand) = match h with 
  | Empty -> Empty
  | Hand ((r', s') as c, h') -> 
     if s = s'
     then Hand(c, extract s h')
     else extract s h'

(* Note: For clarity, I annotated the variable names s and h with their
   corresponding type. This is not necessary since in general OCaml will infer
   their types.
*)
(* extract all spades from hand5 *)
let (spades5:hand) = extract Spades hand5 


(* count: hand -> int 
   count(h) counts the number of cards in a hand h
   
*)

let rec count h = match h with 
  | Empty -> 0
  | Hand (c, h) -> count h + 1

(* find : (rank * hand) -> suit option

   find the first card with rank r in h and 
       return its correpsonding suit s by Some(s)
   if there is no card with rank r,  
      return None

   To write this function we make use of the pre-defined,
   parameterized datatype 'a option.

   type 'a option = None | Some of 'a

*)

 let rec find (r, h) = match h with
  | Empty -> None
  | Hand ((r', s'), h') ->
     if r = r'
     then Some s' 
     else find (r, h') 


(************************************************************************)
(* Lists *)

(* Here is how one might define lists of elements of the same type: *)

type 'a mylist = Nil | Cons of 'a * 'a mylist

(* Some sample values: *)

let list0 : int mylist = Nil
let list1 : int mylist = Cons(1, Nil)
let list2 : int mylist = Cons(2, Cons(1, Nil))
let list3 : int mylist = Cons(3, list2)

let lst0 : float mylist = Nil
let lst1 : float mylist = Cons(3.1, Cons(2.6, lst0))


(* And using the predefined ML lists, we have these values: *)
let rlist1 : float list = [8.6;5.4]
let rlist2 : float list = 8.6::5.4::[]


(* append: 'a list -> 'a list -> 'a list
       append(l1, l2) returns a list consisting of the elements of l1
                      followed by the elements of l2.
   Invariants: none
   Effects: none

   NOTE:  This operation is defined in ML's Standard Basis 
          via the right-associative infix operator "@".

   Observe that the running time is proportional to the length 
   of the first argument.
*)

let rec append l1 l2 = match l1 with 
  | []    -> l2
  | x::l' -> x::append l' l2


let app12  : float list = append rlist1 rlist2
let app12' : float list = rlist1 @ rlist2


(* Remark : Writing functions without pattern matching 
            *** Old style ***
            This requires to write first functions which 
        allow us to take apart lists.
*)

(* head: 'a list -> 'a 

   Note: head may be undefined for the empty list

 let head (h::t) = h;;
           ^^^^^^^^^^
Warning P: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]

*)

let head (h::t) = h

(* tail: 'a list -> 'a list *)
let tail l = match l with 
  | [] -> []
  | h::t -> t

(* Destructor style *)
let rec app (l1, l2) = 
  if l1 = [] then l2
    else 
      head(l1)::(app (tail(l1), l2))



(* Here is a function that reverses a list.
   For example,   rev [1, 2, 3, 4] ==> [4, 3, 2, 1].

   The code below is extremely inefficient.
   Can you see why?
   Next lecture we will see a more efficient implementation.


   let rev : 'a list -> 'a list
       rev(l) returns a list consisting of the elements of l in reverse order.
   Invariants: none
   Effects: none

*)

let rec rev l = match l with 
  | []   -> []
  | x::l -> (rev l) @ [x]


(* What is the tail-recursive version of this? *)

let rev' l = 
  let rec rev_tr l acc = match l with 
    | []   -> acc
    | h::t -> rev_tr t (h::acc)
  in
    rev_tr l []


(************************************************************************)

(* Mergesort *)

(*split lst = (l,l') s.t. lst = merge(l,l')
  
  split: 'a list -> ('a list * 'a list) *)

let rec split l = match l with 
  | []          ->  ([ ],[ ])
  | [h]         ->  ([h],[ ])
  | (h1::h2::t) ->  
    let (left, right) = split t in
      (h1::left, h2::right)


(* merge (l,l') = lst

 assumes l and l' are both sorted (<), 
 returns the sorted combination 

 merge: 'a list * 'a list -> 'a list *)
let rec merge l x = match l, x with 
  | []   , x      -> x
  | x    , []     -> x
  | h::t , h'::t' -> 
    if (h <= h') then 
      h::merge t (h'::t')
    else 
      h'::merge (h::t) t'
    
(* mergeSort: 'a list -> 'a list *)
let rec mergeSort l = match l with 
  | []  -> []
  | [x] -> [x] 
  | lst -> 
    let (slst, slst') = split lst in 
      merge (mergeSort slst)  (mergeSort slst')



