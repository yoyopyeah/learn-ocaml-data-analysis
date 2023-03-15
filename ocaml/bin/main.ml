(* open Yojson.Basic.Util *)
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

let read_file file = 
  Arg.read_arg file
  |> Array.fold_left (fun acc s -> acc ^ s) ""
;;

(* let (hw, _file) = 
  let res = String.split_on_char '/'  Sys.argv.(1)
  in 
  (List.nth res 0, List.nth res 1)
;; *)

let file_content = read_file Sys.argv.(1)
;;

(* To quickly extract ASTs from a raw string: *)
let ast_from_str s = s |> Lexing.from_string |> Parse.implementation ;;

(* let _contains s1 s2 =
  let re = Str.regexp_string s2 in
  try 
     ignore (Str.search_forward re s1 0); 
     true
  with Not_found -> false *)

(* let ast = 
  try _ast_from_str file_content with e -> 
    let msg = Printexc.to_string e in
    if (_contains "Syntaxerr" msg) 
      then raise e
  else raise e;
;; *)

let ast = ast_from_str file_content;;

(* To print out an AST using a raw printer: *)
(* Printast.implementation Format.std_formatter ast ;; *)

(* Write AST to file *)
let oc = open_out "analysis/ast_out" ;;
Printast.implementation (Format.formatter_of_out_channel oc) ast ;;

(* To convert an AST into a pretty string: *)
let pretty_str = Pprintast.string_of_structure ast ;;
(* print_string (pretty_str) ;; *)
let pretty_ast = 
  let arr = Array.of_list (String.split_on_char '\n' (pretty_str ^ "\n(**)")) 
  in
  Array.map (fun s -> 
    if (Str.string_match (Str.regexp {|^let|}) s 0)
    then "(**)\n" ^ s else s) arr
;;

Arg.write_arg "analysis/pretty_ast_out" pretty_ast ;;

(* Sys.command("python3 bin/question_split.py " ^ hw) *)

(* TODO: Add dependency *)

(* Read json *)
(* let json = Yojson.Basic.from_file "analysis/fq.json";;
(* let json_str = Yojson.Basic.pretty_to_string json;; *)
let hw_functions = json |> member hw |> to_list ;;
(* List.iter (Yojson.Basic.pretty_to_string) hw_functions;; *)
let hw_str = List.map (Yojson.Basic.pretty_to_string) hw_functions;;
List.iter print_endline hw_str;; *)