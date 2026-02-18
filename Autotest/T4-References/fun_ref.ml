let r = ref 1 in
let s = ref 2 in
let f x = if x>0 then r else s in
begin
  (f 5) := 7;
  prInt !r
end
