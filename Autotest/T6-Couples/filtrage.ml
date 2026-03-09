let t = 0,1 in
let a = match t with
| 1,1 -> 0
| 0,0 -> 1
| 0,1 -> 2
| _ -> 3
in print_int a;
let a = match t with
| 1,1 -> 0
| 0,0 -> 1
| _ -> 3
in print_int a;
