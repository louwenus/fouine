let f x = if x > 2 then 3 else raise (E 4) in
    let a = try (f 1) with E x -> f 5 in
    prInt a
      
