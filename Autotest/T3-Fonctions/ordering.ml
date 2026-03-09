let pp0 () = print_int 0; (fun x -> x);; 
let pp1 () = print_int 1; (fun x -> x);; 
let pp2 () = print_int 2; (fun x -> x);; 

pp1 () (pp2 ()) (pp0 (pp2 () ()));;
