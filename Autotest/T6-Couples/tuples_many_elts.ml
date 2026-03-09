let t = 1,2,fun x -> x,2*x,3*x;;
let a,b,f = t;;
let t = f a;;
let _,_,a = t;;
print_int a;
print_int b;
