let f x = if x <= 0 then 1 else f (x - 1) in
prInt (f 1)
