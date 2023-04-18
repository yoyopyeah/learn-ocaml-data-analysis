
let ast_from_str s = s |> Lexing.from_string |> Parse.implementation;;

let extract_vname_from_value_binding (vb : Parsetree.value_binding) = 
  match vb with 
  | {pvb_pat =  {ppat_desc= Ppat_var {txt = vname; _}; _}; _} | {pvb_pat = {ppat_desc = Ppat_constraint ({ppat_desc = Ppat_var {txt= vname; _}; _}, _); _}; _}-> Some vname
  | _ -> None
;;
let extract_value_names (item : Parsetree.structure_item) : string list = 
  match item with 
  | {pstr_desc = Pstr_value (_, vbs); _} -> 
    List.filter_map extract_vname_from_value_binding vbs
  | _ -> []
;;

let extract_vexpr_from_value_binding (vb: Parsetree.value_binding) =
  match vb with
  | {pvb_expr = ex; _} -> Some ex
  (* | _ -> None *)
;;

let extract_value_exprs (item: Parsetree.structure_item) : Parsetree.expression list =
  match item with
  | {pstr_desc = Pstr_value (_, vbs); _} ->
    List.filter_map extract_vexpr_from_value_binding vbs
  | _ -> [];;

let extract_top_value_names (item: Parsetree.structure_item) : string option =
  match item with
  | {pstr_desc = Pstr_value (_, vbs); _} ->
    extract_vname_from_value_binding (List.hd vbs)
  | _ -> None;;

let rec used_in_exp (name : string) (e : Parsetree.expression) = 
  match e with 
  | {pexp_desc = Pexp_ident {txt = Longident.Lident pid;_}; _} ->
     begin
       (* print_endline "Hiyaa"; *)
       name = pid  (* base case *)
     end
  | {pexp_desc = Pexp_let (_, vbs, e); _} -> 
    (*check whether fname has been redefined, return false if so *)
    let new_vnames = List.filter_map extract_vname_from_value_binding vbs in 
    if List.exists ((=) name) new_vnames then false
    (* otherwise dive into the body *)
    else
      List.exists (fun ({pvb_expr = exp;_}: Parsetree.value_binding) -> used_in_exp name exp) vbs || used_in_exp name e 
  | {pexp_desc = Pexp_apply (e, args); _} ->
     List.exists (function (_, ae) -> used_in_exp name ae) args || used_in_exp name e
  | {pexp_desc = Pexp_fun (_, _, _, bodexp); _} ->
     used_in_exp name bodexp
  | {pexp_desc = Pexp_function case_list; _} ->      List.exists
       (fun ( {pc_rhs = e; _}: Parsetree.case) -> used_in_exp name e ) case_list
  | {pexp_desc = Pexp_tuple (exp_list); _} ->
     List.exists (fun e -> used_in_exp name e) exp_list
  | {pexp_desc = Pexp_match (exp0, case_list); _} | {pexp_desc = Pexp_try (exp0, case_list); _}->
     used_in_exp name exp0 ||
     List.exists
       (fun ( {pc_rhs = e; _}: Parsetree.case) -> used_in_exp name e ) case_list
  | {pexp_desc = Pexp_constraint (exp, _); _} -> used_in_exp name exp
  | {pexp_desc = Pexp_ifthenelse (cond, exp1, (Some exp2)); _} ->
     used_in_exp name cond || used_in_exp name exp1 || used_in_exp name exp2
  | {pexp_desc = Pexp_sequence (exp1, exp2); _} -> used_in_exp name exp1 || used_in_exp name exp2
  | {pexp_desc = Pexp_array explist; _} -> List.exists (fun e -> used_in_exp name e) explist
  | {pexp_desc = Pexp_field (exp, _); _}  -> used_in_exp name exp
  | {pexp_desc = Pexp_record (declist, _); _} -> List.exists (fun (_, e) -> used_in_exp name e) declist
  | {pexp_desc = Pexp_variant (_, (Some exp)); _} -> used_in_exp name exp
  | {pexp_desc = Pexp_letexception (_, exp); _} -> used_in_exp name exp
  | {pexp_desc = Pexp_assert exp; _} -> used_in_exp name exp
  | {pexp_desc = Pexp_construct (_, (Some exp)); _} -> used_in_exp name exp
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
