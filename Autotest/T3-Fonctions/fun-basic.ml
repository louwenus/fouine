let g = fun x -> x+x in
let f = fun x -> fun y -> x*y in
prInt (f 3 4 + g 5)
	       
