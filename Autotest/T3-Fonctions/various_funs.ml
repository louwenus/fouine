let f = fun x y -> x+y in
let g a b = fun c -> a*b*c in
prInt (f 2 3 + g 2 2 2 * ((fun t u -> t-u) 10 8))
