let f x = if x > 2 then 3 else raise (E 4) in
    let a = try (f 5) with | E n -> n*n in
    prInt a
      
