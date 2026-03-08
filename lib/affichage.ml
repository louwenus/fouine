open Types

(*string if*)
let stif b s =
  if b then
    s
  else
    ""
;;

(* transforme une expression au format string. Récursif. Les sous-expressions sont parenthésées *)
let rec affiche_expr e =
  match e with
  | Cst k -> affiche_val k
  | Var name -> name
  (*Cas spécial de for, qui utilise une fonction de la std avec un identifier illégal*)
  | Call
      ( Call (Call (Call (Var ":for_loop", start), Cst (VI incr)), stop)
      , Cst (Fun (Binding id, body,None)) ) ->
    Printf.sprintf
      "for %s = (%s) %sto (%s) do %s done"
      id
      (affiche_expr start)
      (stif (incr = -1) "down")
      (affiche_expr stop)
      (affiche_expr body)
  | Call (Constructor(name,[]), e2) -> Printf.sprintf "%s (%s)" name (affiche_expr e2)
  | Call (e1, e2) -> Printf.sprintf "(%s) (%s)" (affiche_expr e1) (affiche_expr e2)
  | Let (binding, e1, e2, recursive) ->
    (match binding,e1,recursive with
    (*Ce cas spécial (fonctions mutuellement récursives) et réaffiché avec des and pour que ocaml soit content. Fouine supporterait de l'afficher au format couple*)
    | Constr_p("",plist),Constructor("",elist),true ->
      let andlist = List.map2 (fun p e -> Printf.sprintf "%s = (%s)" (affiche_pat p) (affiche_expr e)) plist elist in
      let andstr = List.fold_left (fun acc next -> acc^"\nand "^next) (List.hd andlist) (List.tl andlist) in
      Printf.sprintf "let rec %s in %s" andstr (affiche_expr e2)
    
    | _ ->  Printf.sprintf "let %s %s = (%s) in (%s)" (stif recursive "rec ") (affiche_pat binding) (affiche_expr e1) (affiche_expr e2)
    )
  | Control_flow (pred, branchs, recursive) ->
    Printf.sprintf
      "%s\n(match (%s) with\n%s%s)"
      (stif recursive "while (")
      (affiche_expr pred)
      (List.fold_left
         (fun acc (pat, ex) ->
            acc
            ^ Printf.sprintf
                "| %s -> (%s)%s\n"
                (affiche_pat pat)
                (affiche_expr ex)
                (stif recursive "; true"))
         ""
         branchs)
      (stif recursive "| _ -> false)\n do () done")
  | Constructor (name, sub) ->
    (match sub with
     | [] -> name
     | l ->
       Printf.sprintf
         "%s (%s)"
         name
         (List.fold_left
            (fun acc e -> Printf.sprintf "%s,(%s)" acc (affiche_expr e))
            (Printf.sprintf "(%s)" (affiche_expr (List.hd l)))
            (List.tl l)))
  | Try (e, branchs) ->
    Printf.sprintf
      "(try (%s) with %s)"
      (affiche_expr e)
      (List.fold_left
         (fun acc (pat, e) ->
            Printf.sprintf "%s| %s -> (%s)\n" acc (affiche_pat pat) (affiche_expr e))
         ""
         branchs)
  | Raise e -> Printf.sprintf "raise (%s)" (affiche_expr e)

and affiche_val v =
  match v with
  | VI k -> string_of_int k
  | Intrinsic (_, name) -> name
  (*cas spécial de function, qui utilise un identifier illégal*)
  | Fun (Binding ":function", Control_flow (Var ":function", branchs, false),_) ->
    Printf.sprintf
      "(function\n%s)"
      (List.fold_left
         (fun acc (pat, ex) ->
            acc ^ Printf.sprintf "| %s -> (%s)\n" (affiche_pat pat) (affiche_expr ex))
         ""
         branchs)
  | Fun (bind, e,_) -> Printf.sprintf "fun %s -> (%s)" (affiche_pat bind) (affiche_expr e)
  | Unit -> "()"
  (*On peut noter que les tuple sont affiché correctement car leur nom de constructeur est vide*)
  | Construct (name, values) ->
    (match values with
     | [] -> name
     | l ->
       Printf.sprintf
         "%s (%s)"
         name
         (List.fold_left
            (fun acc e -> Printf.sprintf "%s,(%s)" acc (affiche_val e))
            (Printf.sprintf "(%s)" (affiche_val (List.hd l)))
            (List.tl l)))

and affiche_pat p =
  match p with
  | Binding name -> name
  | Exact v -> affiche_val v
  | Constr_p (name, pats) ->
    (match pats with
     | [] -> name
     | l ->
       Printf.sprintf
         "%s (%s)"
         name
         (List.fold_left
            (fun acc e -> Printf.sprintf "%s,(%s)" acc (affiche_pat e))
            (Printf.sprintf "(%s)" (affiche_pat (List.hd l)))
            (List.tl l)))
  | Either (p1, p2) -> Printf.sprintf "(%s)|(%s)" (affiche_pat p1) (affiche_pat p2)
;;
