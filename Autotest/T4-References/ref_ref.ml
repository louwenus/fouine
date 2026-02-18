let x = ref 3 in
let r = ref x in
let s = ref (!r) in
(!s := 5; prInt ! !r)

    
