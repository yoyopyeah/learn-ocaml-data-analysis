print_string "\n" ;;

let _python_exec =
  Sys.command("python3 bin/test.py")
;;

(* let _ex_dirpath = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises" *)

(* let file = "/Users/yoyooolo/Coding/COMP400/dune_project/ocaml/fall2021exercises/hw1/template.ml" *)

let test_str = "let distance (x1, y1) (x2, y2) =
  let abs x = if x < 0 then -x else x in
  abs (x2 - x1) + abs (y2 - y1)" ;;

(* To quickly extract ASTs from a raw string: *)
let _ast_from_str s = s |> Lexing.from_string |> Parse.implementation ;;

let ast = _ast_from_str test_str ;;

(* To print out an AST using a raw printer: *)
print_string "=== raw ast ===" ;;
Printast.implementation Format.std_formatter ast ;;

print_endline "=== pretty ast ===" ;;
(* To convert an AST into a pretty string: *)
print_string (Pprintast.string_of_structure ast) ;;
