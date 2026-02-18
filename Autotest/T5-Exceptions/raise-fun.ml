let tr f =
  try f () with E x -> prInt x
in
tr (fun () -> raise (E 5))
