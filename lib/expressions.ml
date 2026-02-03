(* un type pour des expressions arithmétiques simples *)
type expr =
  | Cst of valeur
  | Var of string
  | Call of expr * expr
  | Let of string * expr * expr * bool (*let string = expr in expr, recursive ?*)
  | Control_flow of control_flow

(* sémantique opérationnelle à grands pas *)

(* les valeurs ; pour l'instant ça ne peut être que des entiers *)
and valeur =
  | VI of int
  | VB of bool
  | Fun of string option * expr
    (*Fun(Some(binding),expression) is fun binding -> expression. The None form is fun () -> *)
  | Intrinsic of (valeur -> context -> valeur) * string (*A function defined outside of fouine (in ocaml). Used to implement things like print_int or (+) *)
  | Unit

and context = (string, valeur) Hashtbl.t
and control_flow = expr * (pattern * expr) list * bool
(*Represent every? control flow as a tuple (decide,branch,loop) using the following logic:
  - on evalue decide
  - on prend la première branche ayant un pattern accetptant (avec les eventuels binding correspondant)
  - si loop vaut vrai, on recommence tant qu'on peut prendre une branche, puis on renvoie unit
  - sinon, si on a pris une branche, on renvoie la valeur obtenue, sinon on plante
  *)

and pattern =
  | Binding of string
  | Exact of valeur (*Note: function are forbiden here*)

module StringSet = Set.Make (String)

type stringset = StringSet.t

(* évaluation d'une expression en une valeur *)
let rec open_vars_expr = function
  | Cst k -> open_vars_val k
  | Var name -> StringSet.singleton name
  | Call (e, e') -> StringSet.union (open_vars_expr e) (open_vars_expr e')
  | Let (binding, e1, e2, recursive) ->
    let open_fst =
      if recursive then
        StringSet.remove binding (open_vars_expr e1)
      else
        open_vars_expr e1
    in
    StringSet.union open_fst (StringSet.remove binding (open_vars_expr e2))
  | Control_flow (decide, branchs, _) ->
    List.fold_left
      (fun acc (patt, e) ->
         StringSet.union acc (StringSet.diff (open_vars_expr e) (open_vars_pattern patt)))
      (open_vars_expr decide)
      branchs

and open_vars_val = function
  | VI _ | VB _ | Unit -> StringSet.empty
  | Fun (None, e) -> open_vars_expr e
  | Fun (Some name, e) -> StringSet.remove name (open_vars_expr e)
  | Intrinsic _ -> StringSet.empty

and open_vars_pattern = function
  | Binding b -> StringSet.singleton b
  | Exact _ -> StringSet.empty
;;

exception SyntaxError of string (*User entered a program with a syntax error*)

exception
  InternalError of string (*Something have gone wrong in there. Not the user's fault*)

(*Define an expression that will restore every item in capture list as it is now before yelding e*)
(*Done by wrapping e in a bunch of Let in*)
let rec restore capture_list ctx e : expr =
  match e with
  | Cst (Fun (binding, sub)) ->
    Cst (Fun (binding, restore capture_list ctx sub))
    (*Optimisation: faire descendre directement les captures si on detecte directement un appel de fonction*)
  | e ->
    (match capture_list with
     | [] -> e (*Nothing to do because no var were captured*)
     | binding :: r ->
       (match Hashtbl.find_opt ctx binding with
        | Some value -> restore r ctx (Let (binding, Cst value, e, false))
        | None ->
          raise
            (SyntaxError
               ("Error: variable " ^ binding ^ " is undefined while capturing closure"))))
;;

(*Transform a function into a "closure" by saving all open variable (into let statements directly injected in the function code)*)
let mk_closure value ctx =
  match value with
  | Fun (binding, e) ->
    let capture_list = open_vars_val value in
    Fun (binding, restore (StringSet.to_list capture_list) ctx e)
  | v -> v
;;

(*if pred match pat then return a list of binding defined in pat and their values, wrapped in Some.
  if there is no match then return None*)
let matcher (pred : valeur) (pat : pattern) =
  match pat, pred with
  | Exact v, p ->
    if v = p then
      Some []
    else
      None
  | Binding b, p -> Some [ b, p ]
;;

let rec eval_ctx (e : expr) (ctx : context) =
  match e with
  | Cst (Fun (b, e)) -> mk_closure (Fun (b, e)) ctx
  | Cst k -> k
  | Var name ->
    (match Hashtbl.find_opt ctx name with
     | Some v -> v
     | None ->
       print_string ("Searching for variable " ^ name ^ " when context only define:\n");
       Seq.iter (fun v -> print_string (v^" ")) (Hashtbl.to_seq_keys ctx);
       raise (SyntaxError "Undefined variable"))
  | Call (e1, e2) ->
    let v2 = eval_ctx e2 ctx in
    let v1 = eval_ctx e1 ctx in
    (match v1 with
     | Fun (None, expr) ->
       (match v2 with
        | Unit -> eval_ctx expr ctx
        | _ ->
          raise
            (SyntaxError "Attempting to pass an argument to a callable without binding"))
     | Fun (Some binding, expr) ->
       Hashtbl.add ctx binding v2;
       let v = eval_ctx expr ctx in
       Hashtbl.remove ctx binding;
       v
     | Intrinsic (f, _) -> f v2 ctx
     | _ ->
       raise
         (SyntaxError
            "Attempting to call a value wich is not callable. Maybe you applied to much \
             arguments ?"))
  | Let (let_binding, e1, e2, recursive) ->
    let v1 =
      match e1, recursive with
      | e1, false -> eval_ctx e1 ctx
      | Cst (Fun (f_binding, f_ex)), true ->
        let capture_list = StringSet.remove let_binding (open_vars_val (Fun(f_binding, f_ex))) in
        let cc_ex_restore = restore (StringSet.to_list capture_list) ctx f_ex in
        let cc =
          Fun
            ( f_binding
            , Let (let_binding, Cst (Fun (f_binding, cc_ex_restore)), cc_ex_restore, true)
            )
        in
        cc
      | _, true ->
        raise
          (SyntaxError "Illegal usage of let rec. Only use to define function directly")
    in
    Hashtbl.add ctx let_binding v1;
    let v2 = eval_ctx e2 ctx in
    Hashtbl.remove ctx let_binding;
    v2
  | Control_flow (predicat, branchs, loop) ->
    let pred = eval_ctx predicat ctx in
    (match
       List.find_map
         (fun c -> Option.map (fun l -> l, snd c) (
         matcher pred (fst c)))
         branchs
     with
     | None ->
       if loop then
         Unit
       else
         raise (SyntaxError "A value was not matched by any branch of a control flow")
     | Some (binding_list, branch) ->
       Hashtbl.add_seq ctx (List.to_seq binding_list);
       let v = eval_ctx branch ctx in
       List.iter (fun (name, _) -> Hashtbl.remove ctx name) binding_list;
       if loop then
         eval_ctx e ctx (*As e is the whole control flow, we loop back to evaluating pred*)
       else
         v)
;;

let rec curify lst e =
  match lst with
  | [] -> e
  | id :: r -> Cst (Fun (Some id, curify r e))
;;
