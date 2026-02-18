let f = fun x -> x in
    let g = f f in
 (g (fun z->z) (g 3))
      
