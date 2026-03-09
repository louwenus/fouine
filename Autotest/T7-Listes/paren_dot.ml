let rec f = function
[] -> prInt 0;
|e::l -> prInt e;f l;;

f ((::) (1,(::) (2, 3::( (::) (4,[5;6]) ) ) ) )
