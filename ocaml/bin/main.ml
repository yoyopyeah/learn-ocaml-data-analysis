let file = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises/hw1/template.ml" ;;

let test_str = 
  Arg.read_arg file
  |> Array.fold_left (fun acc s -> acc ^ s) ""
;;

(* To quickly extract ASTs from a raw string: *)
let _ast_from_str s = s |> Lexing.from_string |> Parse.implementation ;;

let ast = _ast_from_str test_str ;;

(* To print out an AST using a raw printer: *)
(* Printast.implementation Format.std_formatter ast ;; *)

(* Write AST to file *)
let oc = open_out "ast_out" ;;
Printast.implementation (Format.formatter_of_out_channel oc) ast ;;

(* To convert an AST into a pretty string: *)
let pretty_str = Pprintast.string_of_structure ast ;;
(* print_string (pretty_str) ;; *)
let pretty_ast = Array.of_list (String.split_on_char '\n' pretty_str) ;;

Arg.write_arg "pretty_ast_out" pretty_ast ;;