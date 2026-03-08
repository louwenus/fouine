open Types
open Affichage

let debug = ref false
let pp = Printf.sprintf

let dbg str =
  if !debug then (
    print_string (Lazy.force str);
    print_newline ()
  )
;;

(* permet de trouver l'ensemble des variable libre dans une expression (utilisé pour les captures de closure) *)
let rec open_vars_expr = function
  | Cst k -> open_vars_val k
  | Var name -> StringSet.singleton name
  | Call (e, e') -> StringSet.union (open_vars_expr e) (open_vars_expr e')
  | Let (binding, e1, e2, recursive) ->
    let open_fst =
      if recursive then
        StringSet.diff (open_vars_expr e1) (open_vars_pattern binding)
      else
        open_vars_expr e1
    in
    StringSet.union
      open_fst
      (StringSet.diff (open_vars_expr e2) (open_vars_pattern binding))
  | Control_flow (decide, branchs, _) ->
    List.fold_left
      (fun acc (patt, e) ->
         StringSet.union acc (StringSet.diff (open_vars_expr e) (open_vars_pattern patt)))
      (open_vars_expr decide)
      branchs
  | Constructor (_, expr_list) ->
    List.fold_left
      (fun acc e -> StringSet.union acc (open_vars_expr e))
      StringSet.empty
      expr_list
  | Try (e, branchs) ->
    StringSet.union
      (open_vars_expr e)
      (List.fold_left
         (fun acc (pat, branch) ->
            StringSet.union
              acc
              (StringSet.diff (open_vars_expr branch) (open_vars_pattern pat)))
         StringSet.empty
         branchs)
  | Raise e -> open_vars_expr e

and open_vars_val = function
  | VI _ | Unit -> StringSet.empty
  | Fun (binding, e, capture_list) ->
    StringSet.diff
      (StringSet.diff (open_vars_expr e) (open_vars_pattern binding))
      (StringSet.of_list (List.map fst (Option.value ~default:[] capture_list)))
  | Intrinsic _ -> StringSet.empty
  | Construct (_, val_list) ->
    List.fold_left
      (fun acc e -> StringSet.union acc (open_vars_val e))
      StringSet.empty
      val_list

and open_vars_pattern = function
  | Binding b -> StringSet.singleton b
  | Exact _ -> StringSet.empty
  | Constr_p (_, pat_list) ->
    List.fold_left
      (fun acc e -> StringSet.union acc (open_vars_pattern e))
      StringSet.empty
      pat_list
  | Either (p1, p2) -> StringSet.inter (open_vars_pattern p1) (open_vars_pattern p2)
;;

(*Base operation on the context: variable and constructor update and retrieval*)
let find_var ctx name =
  match Hashtbl.find_opt ctx.vars name with
  | Some v -> v
  | None -> raise (SyntaxError ("The identifier " ^ name ^ " is undefined"))
;;

let add_var ctx name v =
  match name with
  | "_" -> ()
  | _ ->
    dbg (lazy (pp "adding binding %s with value %s" name (affiche_val v)));
    Hashtbl.add ctx.vars name v
;;

let add_vars ctx seq = Seq.iter (fun (name, v) -> add_var ctx name v) seq

let rem_var ctx name =
  match name with
  | "_" -> ()
  | _ ->
    dbg (lazy (pp "removing binding %s" name));
    Hashtbl.remove ctx.vars name
;;

let rem_vars ctx seq = Seq.iter (rem_var ctx) seq

let add_ctr ctx name arity =
  dbg (lazy (pp "adding constructor %s with arity %i" name arity));
  Hashtbl.add ctx.constructors name arity
;;

let find_ctr ctx name =
  match Hashtbl.find_opt ctx.constructors name with
  | Some v -> v
  | None -> raise (SyntaxError ("The constructor " ^ name ^ " is undefined"))
;;

(*Define an expression that will restore every item in capture list as it is now before yielding e*)
(*Use the closure ability to restore context*)
let rec restore capture_list ctx e : expr =
  match e with
  (*Optimisation: faire descendre directement les captures si on detecte directement un appel de fonction*)
  (*Si ce n'est pas une fonction mais une autre constante, pas besoin de l'environement restoré, c'est ok*)
  | Cst (Fun (binding, Cst interior, None)) | Cst (Fun (binding, Cst interior, Some []))
    -> Cst (Fun (binding, restore capture_list ctx (Cst interior), Some []))
  | Cst (Fun (binding, interior, capture)) ->
    let capture = Option.value ~default:[] capture in
    let capture =
      List.fold_left
        (fun capture name -> (name, find_var ctx name) :: capture)
        capture
        capture_list
    in
    Cst (Fun (binding, interior, Some capture))
  | Cst v -> Cst v
  | Constructor (name, exprs) ->
    Constructor (name, List.map (restore capture_list ctx) exprs)
  | Var _ | Call _ | Control_flow _ | Let _ | Try _ | Raise _ ->
    raise
      (SyntaxError
         ("Cannot do context restore in this expression. rec keywoard can only be used \
           on function definition.\n\
          \ Faulty expr:\n"
          ^ Affichage.affiche_expr e))
;;

(*Transform a function into a closure by saving all open variable)*)
let mk_closure value ctx =
  dbg (lazy (pp "Capturing closure for %s" (affiche_val value)));
  match value with
  | Fun (_, _, _) ->
    let capture_list = open_vars_val value in
    dbg
      (lazy
        (pp
           "Capture list is:%s"
           (StringSet.fold (fun acc s -> acc ^ " " ^ s) capture_list "")));
    (match restore (StringSet.to_list capture_list) ctx (Cst value) with
     | Cst f -> f
     | _ -> raise (InternalError "mk_closure programation error"))
  | v -> v
;;

(*try to match a pattern and a value. If it match, return a list of (binding,value), else, return None*)
let rec matcher (pred : valeur) (pat : pattern) =
  match pat, pred with
  | Exact v, p ->
    if v = p then
      Some [] (*We match, without binding*)
    else
      None
    (*We do not match*)
  | Binding b, p -> Some [ b, p ] (*We always match and bind*)
  | Constr_p (pat_name, pat_list), Construct (cont_name, val_list)
    when pat_name = cont_name ->
    (*Constructor case: must be the same constructor*)
    (try
       List.fold_left2
         (fun acc pat v ->
            Option.bind acc (fun l -> Option.map (fun l2 -> l2 @ l) (matcher v pat)))
            (*Recurisve match, propagating the binding as long as it match, but if one does not match then no one match*)
         (Some [])
         pat_list
         val_list
     with
     | Invalid_argument _ -> None)
    (*Case were both contrsuct does not have the same number of argument (eg:tupple of not the same lenght)*)
  | Constr_p _, _ -> None
  | Either (p1, p2), v ->
    (match matcher v p1 with
     | Some l -> Some l
     | None -> matcher v p2)
;;
let rec expr_matcher e pat =
  match pat,e with
  | Constr_p(pat_name, pat_list), Constructor(cont_name,e_list) when pat_name = cont_name -> (
      try List.fold_left2 (fun acc pat e -> Option.bind acc (fun l -> Option.map (fun l2 -> l2 @ l) (expr_matcher e pat))) (Some []) pat_list e_list
      with Invalid_argument _ -> None
    )
  | _, Cst(v) -> matcher v pat
  | _ -> None
;;


(*Eval an expression in context. CPS style for exeption handling*)
let rec eval_ctx
          (ctx : context)
          (e : expr)
          (cont : valeur -> 'a)
          (cont_exception : valeur -> 'a)
  : 'a
  =
  match e with
  | Cst (Fun (b, e, caplist)) -> cont (mk_closure (Fun (b, e, caplist)) ctx)
  | Cst k ->
    dbg (lazy (pp "%s" (affiche_val k)));
    cont k
  | Var name ->
    let v = find_var ctx name in
    dbg (lazy (pp "Resolved var %s to %s" name (affiche_val v)));
    cont v
  | Call (Constructor (name, []), arg) ->
    (match find_ctr ctx name with
     | 0 -> raise (SyntaxError "A constructor with no argument cannot be called")
     | n ->
       eval_ctx ctx arg (fun v ->
         dbg (lazy (pp "Constructing a %s with arguments %s" name (affiche_val v)));
         match n, v with
         | k, Construct ("", l) when k = -1 || List.length l = k ->
           cont (Construct (name, l))
         | 1, v | -1, v -> cont (Construct (name, [ v ]))
         | _ ->
           raise
             (SyntaxError
                ("constructor " ^ name ^ " called with improper number of arguments"))))
      cont_exception
  | Call (e1, e2) ->
    eval_ctx
      ctx
      e2
      (fun v2 ->
         eval_ctx
           ctx
           e1
           (fun v1 ->
              match v1 with
              | Fun (binding, expr, restore_list) ->
                (match matcher v2 binding with
                 | None ->
                   raise
                     (SyntaxError
                        (Printf.sprintf
                           "Error: A function argument pattern was not matched when \
                            calling %s with %s"
                           (affiche_val v1)
                           (affiche_val v2)))
                 | Some l ->
                   (match expr with
                    | Cst (Fun (bind, expr, inner_list)) ->
                      cont
                        (Fun
                           ( bind
                           , expr
                           , Some
                               (Option.value ~default:[] restore_list
                                @ l
                                @ Option.value ~default:[] inner_list) ))
                    | expr ->
                      dbg
                        (lazy (pp "Calling %s with %s" (affiche_val v1) (affiche_val v2)));
                      add_vars ctx (List.to_seq l);
                      Option.iter (fun l -> add_vars ctx (List.to_seq l)) restore_list;
                      eval_ctx
                        ctx
                        expr
                        (fun v ->
                           rem_vars ctx (Seq.map fst (List.to_seq l));
                           rem_vars
                             ctx
                             (Seq.flat_map
                                (fun l -> List.map fst l |> List.to_seq)
                                (Option.to_seq restore_list));
                           cont v)
                        (fun v ->
                           rem_vars ctx (Seq.map fst (List.to_seq l));
                           rem_vars
                             ctx
                             (Seq.flat_map
                                (fun l -> List.map fst l |> List.to_seq)
                                (Option.to_seq restore_list));
                           cont_exception v)))
              | Intrinsic (f, name) ->
                dbg (lazy (pp "Calling intrinsic %s with arg %s" name (affiche_val v2)));
                cont (f v2 ctx)
              | _ ->
                raise
                  (SyntaxError
                     (Printf.sprintf
                        "Error: attempting to call the non-callable value %s (argument \
                         was %s)"
                        (affiche_val v1)
                        (affiche_val v2))))
           cont_exception)
      cont_exception
  | Let (let_binding, e1, e2, recursive) ->
    let er v1 =
      SyntaxError
        (Printf.sprintf
           "Error: no match while attempting to bind value in let: pattern is %s\n\
           \ and value is %s"
           (affiche_pat let_binding)
           (affiche_val v1))
    in
    let next l =
      add_vars ctx (List.to_seq l);
      eval_ctx
        ctx
        e2
        (fun v2 ->
           rem_vars ctx (Seq.map fst (List.to_seq l));
           cont v2)
        (fun v2 ->
           rem_vars ctx (Seq.map fst (List.to_seq l));
           cont_exception v2)
    in
    if recursive then (
      let capture_list =
        StringSet.diff (open_vars_expr e1) (open_vars_pattern let_binding)
      in
      let cc_ex_restore = restore (StringSet.to_list capture_list) ctx e1 in
      match expr_matcher cc_ex_restore let_binding with
           | None -> raise (SyntaxError ("Malformed let rec: "^(affiche_expr e)))
           | Some l ->
             next
               (List.map
                  (fun (name, v) ->
                     ( name
                     , match v with
                       | Fun (bind, expr, restore) ->
                         Fun (bind, Let (let_binding, cc_ex_restore, expr, true), restore)
                       | _ -> v ))
                  l)
    ) else
      eval_ctx
        ctx
        e1
        (fun v1 ->
           match matcher v1 let_binding with
           | None -> raise (er v1)
           | Some l -> next l)
        cont_exception
  | Control_flow (predicat, branchs, loop) ->
    eval_ctx
      ctx
      predicat
      (fun pred ->
         match
           List.find_map
             (fun c -> Option.map (fun l -> l, snd c) (matcher pred (fst c)))
             branchs
         with
         | None ->
           if loop then (
             dbg (lazy (pp "end of loop"));
             cont Unit
           ) else
             raise (SyntaxError "A value was not matched by any branch of a control flow")
         | Some (binding_list, branch) ->
           dbg (lazy (pp "Successful matching in control flow"));
           add_vars ctx (List.to_seq binding_list);
           eval_ctx
             ctx
             branch
             (fun v ->
                rem_vars ctx (Seq.map fst (List.to_seq binding_list));
                if loop then
                  eval_ctx ctx e cont cont_exception
                (*As e is the whole control flow, we loop back to evaluating pred*)
                else
                  cont v)
             (fun v ->
                rem_vars ctx (Seq.map fst (List.to_seq binding_list));
                cont_exception v))
      cont_exception
  | Constructor (name, expr_list) ->
    (*Here, the case where expr_list is empty and a single argument is provided via call is already handled by the parent Call ast Node*)
    (*So arity should be exactly -1 (any) or equal to the lenght of the provided list (wich can be zero)*)
    let n = List.length expr_list in
    let ar = find_ctr ctx name in
    if ar = -1 || ar = n then (
      let conti =
        ref (fun l ->
          dbg
            (lazy
              (pp
                 "Constructing a %s with arguments %s"
                 name
                 (List.fold_left (fun acc v -> acc ^ ", " ^ affiche_val v) "" l)));
          cont (Construct (name, List.rev l)))
      in
      List.iter
        (fun e ->
           dbg
             (lazy
               (pp
                  "building up the passing closure with expression e = %s"
                  (affiche_expr e)));
           let next = !conti in
           conti := fun l -> eval_ctx ctx e (fun v -> next (v :: l)) cont_exception)
        (List.rev expr_list);
      !conti []
    ) else
      raise
        (SyntaxError ("The constructor " ^ name ^ " has the wrong number of argument"))
  | Try (e, branchs) ->
    dbg (lazy (pp "entering a try"));
    eval_ctx ctx e cont (fun v ->
      dbg (lazy (pp "attempting to catch the error %s" (affiche_val v)));
      match
        List.find_map
          (fun c -> Option.map (fun l -> l, snd c) (matcher v (fst c)))
          branchs
      with
      | None -> cont_exception v
      | Some (bind, branch) ->
        dbg (lazy (pp "catched succesfully"));
        add_vars ctx (List.to_seq bind);
        eval_ctx
          ctx
          branch
          (fun v ->
             rem_vars ctx (Seq.map fst (List.to_seq bind));
             cont v)
          (fun v ->
             rem_vars ctx (Seq.map fst (List.to_seq bind));
             cont_exception v))
  | Raise e ->
    dbg (lazy (pp "Raising an error."));
    eval_ctx ctx e cont_exception cont_exception
;;
