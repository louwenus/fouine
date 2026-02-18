let r = ref 0 in
try
  begin
    r := !r + 1;
    raise (E !r)
  end
with | E n -> prInt (if n=0 then -1 else 55)
                
