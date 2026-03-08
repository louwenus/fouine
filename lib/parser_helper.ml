open Types;;

(*Make a function of several args into a function of a single arg (returning another func)*)
let rec curify lst e =
  match lst with
  | [] -> e
  | p :: r -> Cst (Fun (p, curify r e,None))
;;

(*Transform lists in brackets into a bunch of :: construct*)
let rec list_expr expr_list = match expr_list with
| [] -> Constructor("[]",[]);
| e::l -> Constructor("(::)",[e;list_expr l])

(*The same but for patterns*)
let rec list_patt pat_list = match pat_list with
| [] -> Constr_p("[]",[])
| e::l -> Constr_p("(::)",[e;list_patt l])


(*because for loops hare hard to define, we call a std function wich do it for us*)
let for_loop id start incr stop body =
Call(Call(Call(Call(Var(":for_loop"),start),Cst(VI(incr))),stop),Cst(Fun(Binding(id),body,None)))


(*Handling let declaration (with and wich are reduced to tuple construct)*)
let mk_let_and (rc:bool) (lst: (pattern * (pattern list) * expr) list) (e2:expr) : expr =
  match lst with
  | [(p,plist,e1)] -> Let(p,curify plist e1,e2,rc)
  | lst -> (
      let patterns = List.map (fun (p,_,_) -> p) lst in
      let exprs = List.map (fun (_,plist,e1) -> curify plist e1) lst in
      (*Tupple construct with each element*)
      Let( Constr_p("",patterns), Constructor("",exprs), e2, rc )
    )
