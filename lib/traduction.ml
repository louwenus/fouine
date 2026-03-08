(*
    An unfinished and untested attempt at CPS source traduction
*)

open Types


(******************************************************************
*********** Utility fonction for easy AST generation **************
*******************************************************************)
(*parse a format and its arguments into an expr*)
let ex fmt =
  Printf.ksprintf
    (fun s ->
       let lexbuf = Lexing.from_string s in
       Parser.main Lexer.token lexbuf)
    fmt
;;

(*get and remove head*)
let (!!) rl = match !rl with
  | [] -> failwith "no more values"
  | e::l -> rl:=l;e;;

(*parse a expression, to replace all ocurrence of __ with expressions in rep, in order*)
let rec replace_e (e : expr) (rep : expr list ref) : expr =
  match e with
  | Var "__" -> !!rep
  | Var b -> Var b
  | Cst v -> Cst (replace_v v rep)
  | Call (e1, e2) ->
    let e1 = replace_e e1 rep in
    Call (e1, replace_e e2 rep)
  | Let (binding, e1, e2, recur) ->
    let binding = replace_pat binding rep in
    let e1 = replace_e e1 rep in
    Let (binding,e1, replace_e e2 rep, recur)
  | Control_flow (e, branchs, loop) ->
    let e = replace_e e rep in
    Control_flow
      ( e 
      , List.map (fun (pat, e) -> replace_pat pat rep, replace_e e rep) branchs
      , loop )
  | Constructor (c, lst) -> Constructor (c, List.map (fun x -> replace_e x rep) lst)
  | Raise e -> Raise (replace_e e rep)
  | Try (e, branchs) ->
    let e = replace_e e rep in
    Try (e
      , List.map (fun (pat, e) -> replace_pat pat rep, replace_e e rep) branchs )

(*The same in values*)
and replace_v v rep =
  match v with
  | Construct (c, lst) -> Construct (c, List.map (fun x -> replace_v x rep) lst)
  | Fun (binding, e, None) ->
    let binding = replace_pat binding rep in
    Fun (binding, replace_e e rep, None)
  | Fun (_,_,Some _) -> raise (InternalError "Source code should have no capture")
  | VI _ | Unit | Intrinsic _ -> v

(*And in patterns (limited functionality)*)
and replace_pat p rep =
  match p with
  | Binding "__" ->
    (match !!rep with
     | Cst v -> Exact v
     | _ -> failwith "cant replace a pattern match with a non-constant")
  | Binding _ -> p
  | Exact v -> Exact (replace_v v rep)
  | Constr_p (c, lst) -> Constr_p (c, List.map (fun x -> replace_pat x rep) lst)
  | Either (p1, p2) -> Either (replace_pat p1 rep, replace_pat p2 rep)
;;

(*Combine the above to get the final expr from string, format, and expression list*)
let placeholder fmt =
  Printf.ksprintf
    (fun s v ->
       let lexbuf = Lexing.from_string s in
       let e = Parser.main Lexer.token lexbuf in
       replace_e e (ref v))
    fmt
;;
(*Fonction identité (Ast fouine)*)
let id = Cst(Fun(Binding("v"),Var("v"),None));;

(**********************************************
******* La fonction de traduction *************
***********************************************)

(*pour les expressions*)
let rec cps (e : expr) : expr =
  match e with
  | Var id -> placeholder "fun k k_E -> k __" [Var(id)]
  | Cst v -> placeholder "fun k k_E -> k __" [(Cst (cps_v v))]
  | Let(binding,e1,e2,false) ->
    let e1,e2 = cps e1,cps e2 in
    placeholder "fun k k_E -> __ (fun %s -> __ k k_E) k_E" (Affichage.affiche_pat binding) [e1;e2] (*Oui je transforme un pattern en string pour le reparser. Mais je suis une feignasse*)
  | Let(_,_,_,true) -> failwith "todo"
  | Call(e1,e2) ->
    let e1,e2 = cps e1,cps e2 in
    placeholder "fun k k_E -> __ (fun v2 -> __ (fun v1 -> v1 v2 k k_E) k_E) k_E" [e2;e1]
  | Control_flow(e,branchs,false) ->
    let e = cps e in
    let branchs = List.map (fun (p,e) -> (p,  cps e)) branchs in
    placeholder "fun k k_E -> __ (fun v -> __ k k_E) k_E" [e;Control_flow(Var("v"),branchs,false)]
  | Constructor(name,lst) ->
    let lst = List.map (fun e -> Call(Call(cps e,id),Var("k_E"))) lst in
    placeholder "fun k k_E -> k __" [Constructor(name,lst)]
  | Try(e1,branchs) ->
    let e1,e2 = cps e1,Control_flow(Var("e"),List.map (fun (p,e) -> (p,cps e)) branchs,false) in
    placeholder "fun k k_E -> __ k (fun e -> __ k k_E)" [e1;e2]
  | Raise(e) ->
    let e = cps e in
    placeholder "fun k k_E -> __ k_E k_E" [e]
  | Control_flow(_,_,true) -> failwith "no loop yet"
  
    
(*pour les valeurs*)
and cps_v v =
  match v with
  | Construct (c, lst) -> Construct (c, List.map cps_v lst)
  | Fun (binding, e,None) -> Fun (binding, cps e,None)
  | Fun (_,_,Some _) -> raise (InternalError "Source code should have no capture")
  | VI _ | Unit | Intrinsic _ -> v
;;
