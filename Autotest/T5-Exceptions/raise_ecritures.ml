let ping x =
  raise (E x) in
try
  ping 3
with | E n ->
try
  ping (n+1)
with E k ->
try
  ping (k+1)
with
| (E z) ->
  try
    ping (z+1)
  with
    (E w) -> prInt w
               
