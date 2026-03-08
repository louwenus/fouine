let rec (even,odd) = (
        (fun x -> if x = 0 then true else odd (x-1)),
        (fun x -> if x = 0 then false else even (x-1))
    );;
let pass b =
if b then print_int 1 else print_int 0;;
pass (even 0);;
pass (even 1);;
pass (odd 1);;
pass (even 2);;
pass (odd 3);;
print_newline ()
