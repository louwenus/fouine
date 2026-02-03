open Expressions

(* fonction d'affichage *)
let rec affiche_expr e =
  match e with
  | Cst k -> affiche_val k
  | Var name -> print_string name
  | Call (e1,e2) -> (
      affiche_expr e1;
      print_string " (";
      affiche_expr e2;
      print_char ')';
    )
  | Let(binding,e1,e2,recursive) -> (
    let rc = if recursive then "rec " else "" in
      print_string ("let "^rc^binding^" = ");
      affiche_expr e1;
      print_string " in (";
      affiche_expr e2;
      print_char ')';
    )
  | Control_flow(pred,branchs,recursive) -> (
      if recursive then (print_string "\nwhile there is match do");
      print_string "\nmatch ";
      affiche_expr pred;
      print_string " with\n";
      List.iter (
        fun (pat,ex) ->
        print_char '|';
        affiche_pat pat;
        print_string " -> (";
        affiche_expr ex;
        print_string ")\n"
      ) branchs;
    )
and affiche_val v =
  match v with
  | VI k -> print_int k
  | VB true -> print_string "true"
  | VB false -> print_string "false"
  | Intrinsic (_,name) -> print_string name
  | Fun (bind,e) -> (
      let binding = match bind with | None -> "_" | Some s -> s in
      print_string ("(fun " ^ binding ^ " -> ");
      affiche_expr e;
      print_char ')';
    )
  | Unit -> print_string "()";
and affiche_pat p = match p with
| Binding(name) -> print_string name;
| Exact(v) -> affiche_val v;
;;
