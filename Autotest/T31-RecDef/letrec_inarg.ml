let a = 2 in 
let rec f x = if x=1 then 1 else ((f (x-1))*x) in
let h y = 2*y in 
let g u y = (u y) in 
prInt (g (if a = 2 then f else h) 10)

