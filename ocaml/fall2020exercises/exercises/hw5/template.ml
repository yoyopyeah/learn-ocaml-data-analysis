(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  raise NotImplemented

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) =
    raise NotImplemented
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) =
    raise NotImplemented
  in
  raise NotImplemented

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    raise NotImplemented
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    raise NotImplemented
  in
  raise NotImplemented


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) list =
    raise NotImplemented  
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    raise NotImplemented  
  in
    raise NotImplemented  


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
  raise NotImplemented    