let f f n = if n<2 then 1 else n*(f (n-1))
in
let rec fact x = f fact x
in
prInt (fact 4)
                   
