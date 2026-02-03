(*qmlsdkfjqlkmfdj*)
let (!) x = x-1 in
let rec fibo x =
if x <= 1 then 1
else fibo !x + fibo ! ! x
in
(*let f = fun _ -> print_int (fibo (read_int ())); print_newline (); in
(*  *)
(***)
(* (* *) *)
let rec loop _ =
    f ();
    loop ();
in loop ();*)
print_int (fibo 10);

(* *)


(* **  *())**)
