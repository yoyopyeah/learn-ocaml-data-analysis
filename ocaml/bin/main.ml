(* Command-line Arguments *)
(* let usage_msg = "dune exec ocaml [-hw] <homework number> -i <init flag>"
let hw = ref 0
let init_flag = ref false
let anon_fun _ = ()
let speclist =
  [("-hw", Arg.Set_int hw, "Homework number (1-6) to be processed");
   ("-i", Arg.Set init_flag, "Initialize output structure, process exercises")]
;;

Arg.parse speclist anon_fun usage_msg *)


(* let file = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises/hw1/template.ml" ;; *)
let _dir_path = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises/" ;;

(* let read_file file = 
  Arg.read_arg file
  |> Array.fold_left (fun acc s -> acc ^ s) ""
;; *)

(* let (hw, _file) = 
  let res = String.split_on_char '/'  Sys.argv.(1)
  in 
  (List.nth res 0, List.nth res 1)
;; *)

(* let file_content = read_file Sys.argv.(1)
;; *)

(* To quickly extract ASTs from a raw string: *)
let ast_from_str s = s |> Lexing.from_string |> Parse.implementation ;;

(* find dependencies for each function *)
let dir_deps (a_fun: Parsetree.structure_item) ast =
  List.filter
    (fun b_fun -> Dep.cg_depends a_fun b_fun) ast

let not_self (func: string) dir_deps =
  List.filter (fun b -> func != b) dir_deps

(* for each dep get it's dependencies *)
let rec all_deps_2 fun_name (fun_deps: ((string * Parsetree.structure_item list) list)) =
  let
   deps =  List.assoc fun_name fun_deps 
  in
  let
    names st_itm = List.filter_map (fun x -> x) (List.map (fun x -> Dep.extract_top_value_names x) st_itm)
  in
  (* let dep_names = List.flatten @@ List.map Dep.extract_value_names deps in *)
  let dep_names = names deps in
  let 
    n_s = not_self fun_name dep_names 
  in
    n_s @ List.flatten (List.map (fun x -> all_deps_2 x fun_deps
                                 ) n_s);;


(* create a json structure *)
let build_json fnames fdeps =
  let
    structure = fun deps -> List.map (fun dep -> `String dep) deps
  in
    `Assoc (List.map2 (fun name deps -> (name, `List (structure deps))) fnames fdeps)
;;

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
  let ast = ast_from_str source_code in
  let t_func_names = List.filter_map (fun x -> x) (List.map (fun x -> Dep.extract_top_value_names x) ast) in
  let fun_direct_deps = List.map (fun a -> dir_deps a ast) ast in
  let fun_deps = List.combine t_func_names fun_direct_deps in
  let all_fun_deps = List.map (fun f -> let deps = all_deps_2 f fun_deps in if List.length deps < 1 then [] else deps) t_func_names in
  let json_deps = build_json t_func_names all_fun_deps in
  let json_out = List.hd (String.split_on_char '.' source_file) ^ "_dep.json" in
  let oc = open_out "analysis/ast_out" in
  let pretty_str = Pprintast.string_of_structure ast in
  let pretty_ast =
    let arr = Array.of_list (String.split_on_char '\n' (pretty_str ^ "\n(**)"))
    in
    Array.map (fun s ->
      if (Str.string_match (Str.regexp {|^let|}) s 0)
      then "(**)\n" ^ s else s) arr in
  begin
    Printast.implementation (Format.formatter_of_out_channel oc) ast;
    Arg.write_arg "analysis/pretty_ast_out" pretty_ast;
    Yojson.to_file json_out json_deps;
  end;;
  
print_endline Sys.argv.(1);

work Sys.argv.(1);
