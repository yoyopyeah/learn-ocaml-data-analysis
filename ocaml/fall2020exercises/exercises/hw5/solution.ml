
(* TODO: Write some tests for neighbours. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
              

let g1 = {
  nodes = ["a"; "b" ; "c" ; "d" ; "e"; "f"];
  edges = [("a", "b" , 5);
           ("b", "c", 10);
           ("c", "d", 4);
           ("a", "c", 6);
           ("c" , "b", 1);
           ("d" , "a", 1);
           ("e" , "b", 2)
          ]
  }

let g2 = {
  nodes = ["Montreal"; "Ottawa" ; "Toronto" ; "Vancouver" ; "Iqaluit"];
  edges = [("Montreal", "Ottawa" , 5);
           ("Ottawa", "Toronto", 10);
           ("Toronto", "Vancouver", 4);
           ("Montreal", "Toronto", 6);
           ("Toronto" , "Ottawa", 1);
           ("Vancouver", "Montreal", 1);
           ("Iqaluit" , "Ottawa", 2);           
          ]
} 

let neighbours_tests: ((string graph * string) * (string * int) list) list = [
  ((g1, "a"), [("b", 5); ("c", 6)]);
  ((g1, "b"), [("c", 10)]);
  ((g1, "f"), [])
]    

(* TODO: Implement neighbours. – list neighbours in the same order as encountered in edges *)
(* 15 points *)       
let neighbours g vertex = 
  List.fold_right
    (fun (v1, v2, w) acc ->
      if v1 = vertex then (v2,w) :: acc
      else acc)
    g.edges    []


(* TODO: Implement find_path. *)
(* 20 points *)  
let find_path g a b =
  let rec aux_node (node,w)  visited =
    if node = b then ([b] , w)
    else if List.mem node visited then raise Fail
    else (let (path , cost) = aux_list (neighbours g node) (node :: visited) in 
            (node :: path, cost + w))

  and aux_list nodes visited =
    match nodes with
    | [] -> raise Fail
    | (v,w) :: vs ->
        try
          aux_node (v,w) visited
        with Fail -> aux_list vs visited
  in
  aux_node (a,0) []

(* TODO: Implement find_path'. *)
(* 20 points *)  
let find_path' g a b =
  let rec aux_node (node, w) visited fc sc =
    if node = b then sc ([b], w)
    else if List.mem node visited then fc ()
    else
      aux_list
        (neighbours g node)
        (node :: visited)
        fc
        (fun (path, cost) -> sc (node :: path, cost + w)) 
  and aux_list nodes visited fc sc =
    match nodes with
    | [] -> fc ()
    | v :: vs ->
        aux_node v visited
          (fun () -> aux_list vs visited fc sc)
          sc
  in
  aux_node (a, 0) []
    (fun () -> raise Fail)
    (fun l -> l)


(* 25 points *)  
let find_all_paths g a b =  
  (* aux_node: finds a path from node to b *) 
  let rec aux_node (node,w)  visited =
    if node = b then [([b] , w)]
    else if List.mem node visited then raise Fail
    else (let pc_list  = aux_list (neighbours g node) (node :: visited) in 
          List.map (fun (path,cost) -> (node::path , cost + w)) pc_list )

  and aux_list nodes visited =
    match nodes with
    | [] -> []
    | nw :: vs ->
        try
         (aux_node nw visited  (* if nw = (v,w) then compute path from v to b *)
          @ aux_list vs visited )
        with Fail -> aux_list vs visited
  in
  aux_node (a,0) []

(* maybe not the most beautiful solution, but it's simple and it works ... *)
(* 15 points *)  
let find_shortest_path g a b =
  let rec min l = match l with
    | [] -> None
    | [(p,w)] -> Some (p,w)
    | (p1,w1)::(p2,w2)::rest ->
       if w1 <= w2 then min ((p1,w1)::rest) else min ((p2,w2)::rest)
  in
    min (find_all_paths g a b)  


