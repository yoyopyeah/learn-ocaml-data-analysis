(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
    raise NotImplemented

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
]

(* TODO: Implement decompress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list =
    raise NotImplemented


(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [

]

(* TODO: Implement eval. *)
let rec eval e =
    raise NotImplemented

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
]

(* TODO: Implement to_instr. *)
let rec to_instr e = 
    raise NotImplemented


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
]


(* TODO: Implement to_instr. *)               
let instr i s = 
    raise NotImplemented


(* TODO: Write a good set of tests for prog *)
let prog_tests = [
]

(* TODO: Implement prog. *)
let prog instrs = 
  raise NotImplemented
