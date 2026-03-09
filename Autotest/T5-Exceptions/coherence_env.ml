let rec iter n =
    if n > 1000 then raise (E(1))
    else
    try
        print_int n;
        iter (n+1);
    with
    | E(x) -> print_int n; print_int x; raise (E(x+1))
in
try
iter 10
with
| E(x) -> ignore x;;
