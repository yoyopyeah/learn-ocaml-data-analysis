(* the tuple for a grade event represents the score and number of
   test cases passed *)
type event = Grade of int * int
           | Compile
           | Eval


type err_kind = Syntax
              | Type of string
              | Logic

type err =
  {
    count: int;
    error_kind: err_kind;
  }


type state_node = {
    student_id: string;
    program_text: string;
    errors: err list;
    timestamp: string;
    node_type: event;
    assignment: int;
    file_path: string;
  }


type sort_key = Student
              | Timestamp
              | Event
              | TypeError
              | Score


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


let eq node other_node =
  let comp_errs errs other_errs =
    List.length errs = List.length other_errs
    && List.for_all2
         (fun e1 e2 -> e1.count = e2.count
                     && e1.error_kind = e2.error_kind)
         errs
         other_errs
  in
  String.equal node.student_id other_node.student_id
  && same_program_text node.file_path other_node.file_path
  && comp_errs node.errors other_node.errors
  && node.node_type = other_node.node_type
  && node.assignment = other_node.assignment


