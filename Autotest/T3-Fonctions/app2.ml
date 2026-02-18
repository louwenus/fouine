let f g h x = g (h g x) in
    f (fun z -> z+1) (fun t u -> t u) 3
      
