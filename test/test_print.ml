let rec pr i =
(
    print_int i;
    if i >= 0 then
    pr (i-1)
) in pr 10;
