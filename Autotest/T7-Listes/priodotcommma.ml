let rec f = function
[] -> print_newline ();
| e::l -> print_int e;f l;;

let l = [1;2;let x = 3 in x;4]
in f l;;

let rec len = function
[] -> 0
| x::l -> 1 + (len l);;

let l = [(fun x->x+x);fun x->();x]
in len l;;
