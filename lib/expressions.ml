open Types
open Affichage

let debug = ref false

let dbg fmt =
  Printf.ksprintf
    (fun s ->
       if !debug then (print_string s;
       print_newline ()))
    fmt
;;

(* évaluation d'une expression en une valeur *)
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
  | Fun (binding, e) -> StringSet.diff (open_vars_expr e) (open_vars_pattern binding)
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
    dbg "adding binding %s with value %s" name (affiche_val v);
    Hashtbl.add ctx.vars name v
;;

let add_vars ctx seq = Seq.iter (fun (name, v) -> add_var ctx name v) seq

let rem_var ctx name =
  match name with
  | "_" -> ()
  | _ ->
    dbg "removing binding %s" name;
    Hashtbl.remove ctx.vars name
;;

let rem_vars ctx seq = Seq.iter (rem_var ctx) seq

let add_ctr ctx name arity =
  dbg "adding constructor %s with arity %i" name arity;
  Hashtbl.add ctx.constructors name arity
;;

let find_ctr ctx name =
  match Hashtbl.find_opt ctx.constructors name with
  | Some v -> v
  | None -> raise (SyntaxError ("The constructor " ^ name ^ " is undefined"))
;;

(*Define an expression that will restore every item in capture list as it is now before yielding e*)
(*Done by wrapping e in a bunch of Let in*)
let rec restore capture_list ctx e : expr =
  match e with
  | Cst (Fun (binding, sub)) ->
    Cst (Fun (binding, restore capture_list ctx sub))
    (*Optimisation: faire descendre directement les captures si on detecte directement un appel de fonction*)
  | e ->
    (match capture_list with
     | [] -> e (*Nothing to do because no var were captured*)
     | [ name ] ->
       let value = find_var ctx name in
       Let (Binding name, Cst value, e, false)
     | l ->
       let values = List.map (find_var ctx) l in
       let names = List.map (fun x -> Binding x) l in
       Let (Constr_p ("", names), Cst (Construct ("", values)), e, false))
;;

(*Transform a function into a "closure" by saving all open variable (into let statements directly injected in the function code)*)
let mk_closure value ctx =
  dbg "Capturing closure for %s" (affiche_val value);
  match value with
  | Fun (binding, e) ->
    let capture_list = open_vars_val value in
    dbg "Capture list is:%s" (StringSet.fold (fun acc s -> acc ^ " " ^ s) capture_list "");
    Fun (binding, restore (StringSet.to_list capture_list) ctx e)
  | v -> v
;;

(*if pred match pat then return a list of binding defined in pat and their values, wrapped in Some.
  if there is no match then return None*)
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

(*A constructor pattern with either not the same constructor or not a construct value -> we do not match*)

let rec eval_ctx
          (ctx : context)
          (e : expr)
          (cont : valeur -> 'a)
          (cont_exception : valeur -> 'a)
  : 'a
  =
  match e with
  | Cst (Fun (b, e)) -> cont (mk_closure (Fun (b, e)) ctx)
  | Cst k ->
    dbg "%s" (affiche_val k);
    cont k
  | Var name ->
    let v = find_var ctx name in
    dbg "Resolved var %s to %s" name (affiche_val v);
    cont v
  | Call (Constructor (name, []), arg) ->
    (match find_ctr ctx name with
     | 0 -> raise (SyntaxError "A constructor with no argument cannot be called")
     | n ->
       eval_ctx ctx arg (fun v ->
         dbg "Constructing a %s with arguments %s" name (affiche_val v);
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
              | Fun (binding, expr) ->
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
                   dbg "Calling %s with %s" (affiche_val v1) (affiche_val v2);
                   add_vars ctx (List.to_seq l);
                   eval_ctx
                     ctx
                     expr
                     (fun v ->
                        rem_vars ctx (Seq.map fst (List.to_seq l));
                        cont v)
                     cont_exception)
              | Intrinsic (f, name) ->
                dbg "Calling intrinsic %s with arg %s" name (affiche_val v2);
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
    let next v1 =
      match matcher v1 let_binding with
      | None ->
        raise
          (SyntaxError
             (Printf.sprintf
                "Error: no match while attempting to bind value in let: pattern is %s\n\
                \ and value is %s"
                (affiche_pat let_binding)
                (affiche_val v1)))
      | Some l ->
        add_vars ctx (List.to_seq l);
        eval_ctx
          ctx
          e2
          (fun v2 ->
             rem_vars ctx (Seq.map fst (List.to_seq l));
             cont v2)
          cont_exception
    in
    (match e1, recursive with
     | e1, false -> eval_ctx ctx e1 next cont_exception
     | Cst (Fun (f_binding, f_ex)), true ->
       let capture_list =
         StringSet.diff
           (open_vars_val (Fun (f_binding, f_ex)))
           (open_vars_pattern let_binding)
       in
       let cc_ex_restore = restore (StringSet.to_list capture_list) ctx f_ex in
       let cc =
         Fun
           ( f_binding
           , Let (let_binding, Cst (Fun (f_binding, cc_ex_restore)), cc_ex_restore, true)
           )
       in
       dbg "defined the recursive closure %s" (affiche_val cc);
       next cc
     | _, true ->
       raise
         (SyntaxError "Illegal usage of let rec. Only use to define function directly"))
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
             dbg "end of loop";
             cont Unit
           ) else
             raise (SyntaxError "A value was not matched by any branch of a control flow")
         | Some (binding_list, branch) ->
           dbg "Successful matching in control flow";
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
             cont_exception)
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
            "Constructing a %s with arguments %s"
            name
            (List.fold_left (fun acc v -> acc ^ ", " ^ affiche_val v) "" l);
          cont (Construct (name, List.rev l)))
      in
      List.iter
        (fun e ->
           dbg "building up the passing closure with expression e = %s" (affiche_expr e);
           let next = ! conti in
           conti := (fun l ->
           eval_ctx ctx e (fun v ->
            next (v :: l)) cont_exception))
        (List.rev expr_list);
      !conti []
    ) else
      raise
        (SyntaxError ("The constructor " ^ name ^ " has the wrong number of argument"))
  | Try (e, branchs) ->
    dbg "entering a try";
    eval_ctx ctx e cont (fun v ->
      dbg "attempting to catch the error %s" (affiche_val v);
      match
        List.find_map
          (fun c -> Option.map (fun l -> l, snd c) (matcher v (fst c)))
          branchs
      with
      | None -> cont_exception v
      | Some (bind, branch) ->
        dbg "catched succesfully";
        add_vars ctx (List.to_seq bind);
        eval_ctx
          ctx
          branch
          (fun v ->
             rem_vars ctx (Seq.map fst (List.to_seq bind));
             cont v)
          cont_exception)
  | Raise e -> dbg "Raising an error."; eval_ctx ctx e cont_exception cont_exception
;;
