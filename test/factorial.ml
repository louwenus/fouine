let factorial i =
let cnt = ref 1 in
for k = 1 to i do
    cnt := !cnt * k
done;!cnt
in
print_int (factorial 10)
