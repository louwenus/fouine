let rec factorial_cps n k =
    match n with
    | 0 | 1 -> k 1
    | _ -> factorial_cps (n-1) (fun x -> k (n*x));;

let test n =
print_int (factorial_cps n (fun x -> x));;

test 4;
test 16;
