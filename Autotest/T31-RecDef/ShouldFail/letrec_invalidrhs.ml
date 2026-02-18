let rec f = (let x = f 0 in fun y -> 0+x  )
in f 3
  

