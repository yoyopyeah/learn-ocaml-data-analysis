exception NotImplemented
exception Fail
(* The type of graphs. *)
type weight = int

type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a * weight) list
}

(* --------Hamming numbers------------ *)


type 'a susp = Susp of (unit -> 'a) ;;
let force (Susp s) = s () ;;
let delay f = Susp f ;;

type 'a str = {hd: 'a  ; tl : ('a str) susp}

(* map : ('a -> 'b) -> 'a str -> 'b  str *)
let rec map f s =
  { hd = f (s.hd) ;
    tl = Susp (fun () -> map f (force s.tl))
  }

(* times : int -> int str -> int str *)
let times k = map (fun x -> x * k) ;;
