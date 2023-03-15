(* let s2 = "
let bar x = 100 < x ;;
let foo y = 1 + 321;;
let taz = (foo 1) + (foo 2);;
let f = taz + taz ;;
";; *)

(* ---- Meeting notes ---- *)
(* handle and definitions *)
(* check for pretty printing, functions not always the same, the input can be formatted *)
(* write our own pretty printer? to capture what student wrote we might write our own printer? *)
(* issue with `let...and` definitions  *)

(* let s3 = " *)
(* let foo1 x = x + x  *)
(* and foo2 y = y + y  *)
(* and foo3 z = z + z;; *)
(* ";; *)

let ast_from_str s = s |> Lexing.from_string |> Parse.implementation;;

(* let ast = ast_from_str s2;; *)

(* raw printer *)
(* let () = Printastx.implementation Format.std_formatter ast;; *)

(* pretty printer *)
(* let ast_pretty_str = Pprintast.string_of_structure ast ;; *)

(* let () = Printf.printf "ast_str is %s " ast_pretty_str;; *)
(* let () = Printf.printf "End\n";; *)

(* let dbg_out_exp e = Printast.expression 0 Format.std_formatter e;; *)

(* let last = List.hd (List.rev ast);; *)

(* Goal: *)
(*    get all funs or let declarations depended on *)

(* Printf.printf "last depends on the following structure_itmes: \n" ;; *)
(* List.map *)
(*     (fun x -> if Dep.cg_depends last x then Printf.printf "%s \n" (Pprintast.string_of_structure [x]) else ()) *)
(*     ast;; *)

(* find dependencies for each function *)
let dir_deps (a_fun: Parsetree.structure_item) ast =
   List.filter
    (fun b_fun -> Dep.cg_depends a_fun b_fun) ast
;;

(* let fun_direct_deps = List.map (fun a -> dir_deps a ast) ast;; *)
(*
   * Goal -> add the dependencies of all dependencies
   *)
let not_self (func: string) dir_deps =
  List.filter (fun b -> func != b) dir_deps
  
(* let function_names = List.concat @@ List.map  Dep.extract_value_names  ast;; *)

(* for each dep get it's dependencies *)
let rec all_deps_2 fun_name (fun_deps: ((string * Parsetree.structure_item list) list)) =
  let
   deps =  List.assoc fun_name fun_deps 
  in
  let dep_names = List.flatten @@ List.map Dep.extract_value_names deps in
  let 
    n_s = not_self fun_name dep_names 
  in
    n_s @ List.flatten (List.map (fun x -> all_deps_2 x fun_deps
                                 ) n_s);;

(*    List.map
      (fun other_fun ->
         all_deps_2 (Dep.extract_vname_from_value_binding other_fun)
                     fun_deps
                     (n_s @ ds)) 
         n_s*)
(* let indir_deps (a_fun: string) (dir_deps: (string * Parsetree.structure_item) list) = *)
(*   List.filter (fun x -> match List.assoc_opt a_fun ) dir_deps;; *)

(* let fun_all_deps = List.map (fun a -> *)

(* let rec all_deps x node sast = *)
(*   if Dep.cg_depends x node *)
(*   then node :: (all_deps node sast) *)
(*   else []) *)

(*   List.map *)
(*     (fun x -> *)
(*        if Dep.cg_depends last x *)
(*        then x *)
(*        else _) *)
(*     ast;; *)

(* if Dep.called_in "foo" last then Printf.printf "foo is used";; *)

(* let fun_deps = List.combine function_names fun_direct_deps;; *)

(* let fun_1_deps = List.assoc (List.hd (List.rev function_names)) fun_deps;; *)

(* let fun_1_all_deps = all_deps_2 (List.hd (List.rev function_names)) fun_deps;; *)
(* let build_json_structure = *)
(*   List.map (fun f_name fdeps -> `Assoc [(f_name, `List ((fun dep -> `String dep) fdeps))]);; *)

(* let all_fun_deps = List.map (fun f -> let deps = all_deps_2 f fun_deps in if List.length deps < 1 then [] else deps *)
(* ) function_names;; *)


(* let build_json_structure = *)
(*   List.map (fun f_name fdeps -> `Assoc [(f_name, `List ((fun dep -> `String dep) fdeps))]);; *)

(*   List.map *)
(*     (fun x -> *)
(*        if Dep.cg_depends last x *)
(*        then x *)
(*        else _) *)
(*     ast;; *)

(* if Dep.called_in "foo" last then Printf.printf "foo is used";; *)

(* let fun_deps = List.combine function_names fun_direct_deps;; *)

(* let fun_1_deps = List.assoc (List.hd (List.rev function_names)) fun_deps;; *)

(* let fun_1_all_deps = all_deps_2 (List.hd (List.rev function_names)) fun_deps;; *)

(* let all_fun_deps = List.map (fun f -> let deps = all_deps_2 f fun_deps in if List.length deps < 1 then [] else deps *)
(* ) function_names;; *)


(* let build_json_structure = *)
(*   List.map (fun f_name fdeps -> `Assoc [(f_name, `List ((fun dep -> `String dep) fdeps))]);; *)

let build_json fnames fdeps =
  let
    structure = fun deps -> List.map (fun dep -> `String dep) deps
  in
    `Assoc (List.map2 (fun name deps -> (name, `List (structure deps))) fnames fdeps)
  ;;

(* let json_deps = build_json function_names all_fun_deps;; *)
(* let () = (Printf.printf "%s\n") @@ string_of_int (List.length function_names);;
let () = (Printf.printf "%s\n") @@ string_of_int (List.length @@ List.hd fun_direct_deps);; *)
(* let () = (Printf.printf "%s\n") @@ string_of_int (List.length @@ fun_1_deps);; *)
(* let _ = Printf.printf("The dependencies of f are: \n");;
let () = List.iter (Printf.printf "- %s\n") @@ fun_1_all_deps;; *)
(* let () = List.iter2 (fun f d ->  *)
(*   begin *)
(*     (\* write to a json file *\) *)
(*     (\* Yojson.to_file *\) *)
(*     Printf.printf "The dependencies of %s are: \n" f; *)
(*     List.iter (Printf.printf"- %s\n") d *)
(*   end *)
(*   ) function_names all_fun_deps;; *)
(* let _ = Printf.printf "%d\n" @@ List.length (List.assoc (List.hd function_names) fun_deps);; *)
(* let dep_graph sast = List.fold_left (fun x y -> x :: y) [] sast;; *)
(* Yojson.to_file "deps.json" (build_json_structure function_names fun_deps);; *)
(*
  Goal:    
  main.exe  -f f1,f2,f3  source_code_file
*)

(* print the dependencies in a json file *)
(* let () = Yojson.to_file "out.json" json_deps;; *)

let read_whole_file fpath =
  let ic = open_in fpath in
  let len = in_channel_length ic in 
  let content = really_input_string ic len in
  close_in ic;
  content
;;

let _dump_ast outname (ast : Parsetree.structure_item) = 
  let oc = open_out outname in 
  output_string oc (Pprintast.string_of_structure [ast]);
  close_out oc
;;

let _check fs ast_list = 
  let vnames = List.flatten @@ List.map Dep.extract_value_names ast_list in 
  (* check if all functions are availabe at the top level *)
  List.map (fun f -> 
      if List.exists ((=) f) vnames then Printf.printf "%s definition can be found\n" f 
      else Printf.printf "failed to find definition of %s\n" f
  ) fs 
;;

let work source_file =
  let source_code = read_whole_file source_file in 
  (* let ast_list = Dep.ast_from_str source_code in  *)
  let ast = ast_from_str source_code in
  (* let _ = check fs ast_list in *)
  (* let first = List.hd (List.rev ast_list) in 
  dump_ast "dummpy.ml"  first *)
  let function_names = List.concat @@ List.map  Dep.extract_value_names  ast in
  let fun_direct_deps = List.map (fun a -> dir_deps a ast) ast in
  let fun_deps = List.combine function_names fun_direct_deps in
  let all_fun_deps = List.map (fun f -> let deps = all_deps_2 f fun_deps in if List.length deps < 1 then [] else deps) function_names in
  let json_deps = build_json function_names all_fun_deps in
  let output_file = List.hd (String.split_on_char '.' source_file) ^ ".json" in
  begin
    (* print_endline (string_of_int (List.length function_names));
    Yojson.pp Format.std_formatter json_deps; *)
    Yojson.to_file output_file json_deps;
  end
  

  (* core vs batteries *)
;;

open Cmdliner
(* https://ocaml.org/p/cmdliner/1.1.0/doc/Cmdliner/Arg/index.html *)
(* https://ocaml.org/p/cmdliner/1.1.0/doc/examples.html *)
let src_file = 
  let doc = "source code file" in 
  let docv = "src_file" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv ~doc)

(* let top_defs = 
  let doc = "functions to be extracted in the top level" in 
  let docv = "funs" in
  Arg.(value & opt (list string) [] & info ["f"; "functions"] ~docv ~doc) *)

let cmd = 
  let doc = "split OCaml source code into independent parts" in 
  let man = [
    `S Manpage.s_description; 
    (* `P "Perform simple data dependency analysis, and then split OCaml code into independent parts" *)
    `P "Perform simple data dependency analysis, and then write a json file showing functions and their dependencies."
  ] in
  let info = Cmd.info Sys.argv.(0) ~version:"0.1" ~doc ~man in 
  Cmd.v info Term.(const work $ src_file)

let main () = exit (Cmd.eval cmd)
let () = main ()
