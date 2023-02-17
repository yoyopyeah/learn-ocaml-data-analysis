
let s2 = "
let bar x = 100 < x ;;
let foo y = bar 321;;
let taz = (foo 1) + (foo 2);;
let f = taz + taz ;;
";;

let s3 = "
let foo1 x = x + x 
and foo2 y = y + y 
and foo3 z = z + z;;
";;

let ast_from_str s = s |> Lexing.from_string |> Parse.implementation;;

let ast = ast_from_str s2;;

(* raw printer *)
let () = Printast.implementation Format.std_formatter ast;;

(* pretty printer *)
let ast_pretty_str = Pprintast.string_of_structure ast ;;

let () = Printf.printf "ast_str is %s " ast_pretty_str;;
let () = Printf.printf "End\n";; 
(* 
let dbg_out_exp e = Printast.expression 0 Format.std_formatter e;;
*)

let last = List.hd (List.rev ast);;

Printf.printf "last depends on the following structure_itmes: \n" ;;
List.map 
    (fun x -> if Dep.cg_depends last x then Printf.printf "%s \n" (Pprintast.string_of_structure [x]) else ())
    ast;;

if Dep.called_in "foo" last then Printf.printf "foo is used";;


(*
  Goal:    
  main.exe  -f f1,f2,f3  source_code_file
*)

let read_whole_file fpath =
  let ic = open_in fpath in
  let len = in_channel_length ic in 
  let content = really_input_string ic len in
  close_in ic;
  content
;;

let dump_ast outname (ast : Parsetree.structure_item) = 
  let oc = open_out outname in 
  output_string oc (Pprintast.string_of_structure [ast]);
  close_out oc
;;

let check fs ast_list = 
  let vnames = List.flatten @@ List.map Dep.extract_value_names ast_list in 
  (* check if all functions are availabe at the top level *)
  List.map (fun f -> 
      if List.exists ((=) f) vnames then Printf.printf "%s definition can be found\n" f 
      else Printf.printf "failed to find definition of %s\n" f
  ) fs 
;;

let work source_file fs =
  let source_code = read_whole_file source_file in 
  let ast_list = Dep.ast_from_str source_code in 
  let _ = check fs ast_list in
  let first = List.hd (List.rev ast_list) in 
  dump_ast "dummpy.ml"  first

  (* core vs batteries *)
;;

open Cmdliner
(* https://ocaml.org/p/cmdliner/1.1.0/doc/Cmdliner/Arg/index.html *)
(* https://ocaml.org/p/cmdliner/1.1.0/doc/examples.html *)
let src_file = 
  let doc = "source code file" in 
  let docv = "src_file" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv ~doc)

let top_defs = 
  let doc = "functions to be extracted in the top level" in 
  let docv = "funs" in
  Arg.(value & opt (list string) [] & info ["f"; "functions"] ~docv ~doc)

let cmd = 
  let doc = "split OCaml source code into independent parts" in 
  let man = [
    `S Manpage.s_description; 
    `P "Perform simple data dependency analysis, and then split OCaml code into independent parts"
  ] in
  let info = Cmd.info Sys.argv.(0) ~version:"0.1" ~doc ~man in 
  Cmd.v info Term.(const work $ src_file $ top_defs)

(* let main () = exit (Cmd.eval cmd)
let () = main () *)
