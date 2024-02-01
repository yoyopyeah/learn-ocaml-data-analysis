let same_program_text program other_program =
  (* run code diffing tool on the two programs and observe the output *)
  let rec read_all channel =
    try
      let
        curr_line = input_line channel
      in
      curr_line :: read_all channel
    with End_of_file -> []
  in
  let channel =
    Unix.handle_unix_error
      (fun _ -> Unix.open_process_in ("gumtree textdiff " ^ program ^ " " ^ other_program))
      ()
  in
  let output = read_all channel in
  let contains_one_of slist s2 =
    let regexp_list = List.map (fun x -> Str.regexp_string x) slist in
    List.filter_map
      (fun regexp ->
        try
          Some (Str.search_forward regexp s2 0)
        with Not_found -> None)
      regexp_list
  in
  List.length
    (List.filter
       (fun x -> List.length (contains_one_of ["insert";"update";"remove";"replace"] x) > 0)
       output) = 0
