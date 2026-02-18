let r = ref 3 in
    let u = ref (fun z -> !r) in
    let t = ref  (fun x -> (!u 3)) in 
    prInt (!t 12)
      
