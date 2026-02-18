let f g h x = g (h (g x)) in
    f (fun x -> x+1) (fun y -> y*y) 3
      
