#warnings "+20+26+27+32+39"

(* Instrument the List and tabulate functions to count how many times
   they are used.

   Hide the actual counters from the outside, so even if students
   find our additions to the List module, they can't increment the counters.
*)
let counter () =
  let counter = ref 0 in
  let get () = begin
      let x = !counter in counter := 0; x
    end
  in
  (counter, get)

module Tab:
  sig
    val tabulate: (int -> 'a) -> int -> 'a list
    val get_tab: unit -> int
  end = struct
  let tab_counter, get_tab = counter ()
  let tabulate f n = incr tab_counter; tabulate f n
end

let tabulate = Tab.tabulate

module type List_type = sig
  val length : 'a list -> int
  val compare_lengths : 'a list -> 'b list -> int
  val compare_length_with : 'a list -> int -> int
  val cons : 'a -> 'a list -> 'a list
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val nth_opt : 'a list -> int -> 'a option
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right2 :
    ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> 'a list -> bool
  val memq : 'a -> 'a list -> bool
  val find : ('a -> bool) -> 'a list -> 'a
  val find_opt : ('a -> bool) -> 'a list -> 'a option
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val assoc : 'a -> ('a * 'b) list -> 'b
  val assoc_opt : 'a -> ('a * 'b) list -> 'b option
  val assq : 'a -> ('a * 'b) list -> 'b
  val assq_opt : 'a -> ('a * 'b) list -> 'b option
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val split : ('a * 'b) list -> 'a list * 'b list
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val get_map : unit -> int
  val get_map2: unit -> int
  val get_fold_left : unit -> int
  val get_fold_right : unit -> int
  val get_for_all : unit -> int
end

module List: List_type = struct
  include List

  let map_counter, get_map = counter ()
  let map f l = incr map_counter; map f l

  let map2_counter, get_map2 = counter ()
  let map2 f l1 l2 = incr map2_counter; map2 f l1 l2

  let fl_counter, get_fold_left = counter ()
  let fold_left f init l = incr fl_counter; fold_left f init l

  let fr_counter, get_fold_right = counter ()
  let fold_right f init l = incr fr_counter; fold_right f init l

  let fa_counter, get_for_all = counter ()
  let for_all p l = incr fa_counter; for_all p l
end

let (:=) _ _ = failwith "FORBIDDEN: You should not use := in this exercise."
let ignore _ =
  failwith "FORBIDDEN: You should not use this function in this exercise."
