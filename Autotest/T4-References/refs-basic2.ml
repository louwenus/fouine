let r = ref 3 in
let f g x =
  let res = g x in
  r:= x;
  res
in
let k = f (fun x -> x*x) 12 in
prInt (!r + k)
