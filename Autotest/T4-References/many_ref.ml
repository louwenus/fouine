let l = ref [];;
for i = 0 to 100000 do
    print_newline ();
    let r = ref i in
        l := r::(!l)
done;;
let rec augment l = match l with
| [] -> ()
| e::l -> e := !e + 10; augment l;;
let rec print l = match l with
| [] -> ()
| e::l -> print_int !e; print l;;
augment !l;;
print !l;;
augment !l;;
print !l;;
