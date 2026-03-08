let rec odd x = if x = 0 then true else even (x-1)
and even x = if x=0 then false else odd (x-1);;
let print_bool b = if b then print_int 1 else print_int 0;;

for i = 0 to 20 do
    print_bool (odd i)
done
