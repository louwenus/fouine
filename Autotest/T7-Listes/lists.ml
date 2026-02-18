let rec f l = match l with
  | []   -> prInt(0)
  | h::q -> prInt(h); f q
in
let l = [1;2;3;4;5;66] in
let a::b = l in
f b
