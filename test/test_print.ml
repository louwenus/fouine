let rec affiche_list l = match l with
| [] -> ()
| e::l -> print_int e;affiche_list l;
in affiche_list [1;2;3;4;5;6;7;8;9];;
let Some _ | None = Some 1 in 2;;
