let u = 4 -3 in
let a = if 0 < u then 1 else 0 in
let b = if 0 <= u then 1 else 0 in
let c = if u > 0 then 1 else 0 in
let d = if u >= 0 then 1 else 0 in
let e = if u <> 0 then 1 else 0 in
let f = if u = 1 then 1 else 0 in
let r = a * b * c * d * e * f in
prInt r
