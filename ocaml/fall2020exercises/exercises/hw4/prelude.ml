exception NotImplemented
exception Fail

exception Msg of string

type passwd = string
type bank_account = {update_passwd  : passwd -> passwd -> unit;
                     retrieve       : passwd -> int -> unit;
                     deposit        : passwd -> int -> unit;
                     print_balance  : passwd -> int }

(* Bank account errors *)
let wrong_pass = Msg "Wrong Password"
let too_many_attempts = Msg "Change your password"
let no_money = Msg "Insufficient funds"


let rec fib n = if n = 0 then 0
                else (if n = 1 then 1 else fib (n-2) + fib (n-1))
     
type fib_result =
  { num_rec : int;
    result  : int }

type stats =
  { entries : int ref;
    lkp : int ref }

let store : (int, int) Hashtbl.t = Hashtbl.create 1000


