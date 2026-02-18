let c = ((fun x -> x), fun y -> y)
    in let (f,s) = c in
       prInt (f (f 3))
