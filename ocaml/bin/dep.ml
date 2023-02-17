
let ast_from_str s = s |> Lexing.from_string |> Parse.implementation;;

let extract_vname_from_value_binding (vb : Parsetree.value_binding) = 
  match vb with 
  | {pvb_pat =  {ppat_desc= Ppat_var {txt = vname; _}; _}; _} -> Some vname
  | _ -> None
;;
let extract_value_names (item : Parsetree.structure_item) : string list = 
  match item with 
  | {pstr_desc = Pstr_value (_, vbs); _} -> 
    List.filter_map extract_vname_from_value_binding vbs
  | _ -> []
;;

let rec used_in_exp (name : string) (e : Parsetree.expression) = 
  match e with 
  | {pexp_desc = Pexp_ident {txt = Longident.Lident pid;_}; _} -> name = pid  (* base case *)
  | {pexp_desc = Pexp_let (_, vbs, e); _} -> 
    (*check whether fname has been redefined, return false if so *)
    let new_vnames = List.filter_map extract_vname_from_value_binding vbs in 
    if List.exists ((=) name) new_vnames then false
    (* otherwise dive into the body *)
    else used_in_exp name e 
  | {pexp_desc = Pexp_apply (e, args); _} -> 
    List.exists (function (_, ae) -> used_in_exp name ae) args || used_in_exp name e
  | _ -> false
;;

let called_in (fname : string) (item : Parsetree.structure_item) = 
  match item with 
  | {pstr_desc = Pstr_value (_, vbs); _} -> 
    List.exists 
      (function ({pvb_expr = e; _ } : Parsetree.value_binding) -> used_in_exp fname e)
      vbs
  | {pstr_desc = Pstr_eval(e, _); _} -> used_in_exp fname e
  | _ -> false
;;

let cg_depends (a : Parsetree.structure_item) (b: Parsetree.structure_item) =   
  a == b || List.exists (fun x -> called_in x a) (extract_value_names b)
;;
