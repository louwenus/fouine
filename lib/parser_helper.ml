open Types;;
let rec curify lst e =
  match lst with
  | [] -> e
  | p :: r -> Cst (Fun (p, curify r e))
;;

let rec list_expr expr_list = match expr_list with
| [] -> Constructor("[]",[]);
| e::l -> Constructor("(::)",[e;list_expr l])

let rec list_patt pat_list = match pat_list with
| [] -> Constr_p("[]",[])
| e::l -> Constr_p("(::)",[e;list_patt l])

let for_loop id start incr stop body =
Call(Call(Call(Call(Var(":for_loop"),start),Cst(VI(incr))),stop),Cst(Fun(Binding(id),body)))
