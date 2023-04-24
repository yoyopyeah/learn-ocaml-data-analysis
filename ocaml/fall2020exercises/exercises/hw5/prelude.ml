exception NotImplemented
exception Fail
(* The type of graphs. *)
type weight = int
            
type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a * weight) list
}
