(* let file = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises/hw1/template.ml" ;; *)
let dir_path = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises/"

let read_file file = 
  Arg.read_arg file
  |> Array.fold_left (fun acc s -> acc ^ s) ""
;;

let (hw, _file) = 
  let res = String.split_on_char '/'  Sys.argv.(1)
  in 
  (List.nth res 0, List.nth res 1)
;;

print_endline(hw)

let file_content = read_file (dir_path ^ Sys.argv.(1))
;;

(* To quickly extract ASTs from a raw string: *)
let _ast_from_str s = s |> Lexing.from_string |> Parse.implementation ;;

let ast = _ast_from_str file_content ;;

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

Sys.command("python3 analysis/question_split.py " ^ hw)