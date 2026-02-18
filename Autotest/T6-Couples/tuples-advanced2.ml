let y = 3 in let f y = y*3 in
let x = (1,2), f y in
 let a,b = x in
 let a1, a2 = a in
 prInt (a1 + a2 + b)
