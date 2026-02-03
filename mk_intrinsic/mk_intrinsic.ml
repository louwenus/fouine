open Ppxlib
open Ast_builder.Default


(*Recognised form of type annotation*)
type simple_ty =
  | Ty_ident of string
  | Ty_fun of string * simple_ty

(*Convert from a full-fldged type annotation to a simple_ty one*)
let rec classify_type (ty : Parsetree.core_type) : simple_ty =
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = lid; _ }, []) ->
      Ty_ident (Longident.name lid)

  | Ptyp_arrow (_, arg_ty, ret_ty) ->
      begin
        match arg_ty.ptyp_desc with
        | Ptyp_constr ({ txt = lid; _ }, []) ->
            Ty_fun (Longident.name lid, classify_type ret_ty)
        | _ ->
            Location.raise_errorf ~loc:arg_ty.ptyp_loc
              "mk_intrinsic: function argument must be a type identifier only"
      end

  | _ ->
      Location.raise_errorf ~loc:ty.ptyp_loc
        "mk_intrinsic: unsupported type form, it must be a (simple) function"

(*Simple helper to turn a "string" into the expression   string    (just the identifier denoted by this string)*)
let str_to_expr loc (name:string) =
pexp_ident ~loc (Location.{ txt = (Longident.parse name) ;loc });;

(* Transform a function type into the corresponding recursive Intrinsic (by calling the partial function) (Actually output the AST of that obviously)*)
let rec recursive_transfo ~loc (ty:simple_ty) : Parsetree.expression = match ty with
| Ty_ident ty_name -> str_to_expr loc ("from_"^ty_name) (*Final converions to a value*)
| Ty_fun (arg_ty_name,returned_ty) ->
    let sub = recursive_transfo ~loc returned_ty in
    let to_fn = str_to_expr loc ("to_"^arg_ty_name) in
    [%expr partial name [%e to_fn] [%e sub]]

    
    
;;
(*Take a function/operator name (an identifier) and its type, and return (the AST of a expression that evaluate into) the corresponding fouine Intrinsic *)
let transform
    ~loc
    (name : Longident.t Location.loc)
    (ty_expr : Parsetree.core_type)
  : Parsetree.expression =
    let ident_expr = pexp_ident ~loc: name.loc name in
    let sub = recursive_transfo ~loc (classify_type ty_expr) in
    let name_str = estring ~loc:name.loc (Longident.name name.txt) in
    [%expr let f = [%e ident_expr] in let name = [%e name_str] in [%e sub] f]
;;


(* The actual implem of the mk_intrinsic macro *)
let expand_mk_intrinsic ~ctxt entries =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let exprs = List.map (fun (name, ty) -> transform ~loc name ty) entries in
  elist ~loc exprs
;;

open Ast_pattern
(*match a (identifier : type annotation), extracting both of them*)
let single_entry =
    map
    (pexp_constraint (pexp_ident __') __)
    ~f:(fun f ident ty -> f (ident,ty))

(* match list of these *)
let list_pattern =
    Ast_pattern.(single_expr_payload (elist (single_entry)))


(* Actually register the mk_intrinsic macro using all of the above *)
let mk_intrinsic_ext =
  Extension.V3.declare
    "mk_intrinsic"
    Extension.Context.expression
    list_pattern
    expand_mk_intrinsic;;
let () =
  Driver.register_transformation
    ~rules:[Context_free.Rule.extension mk_intrinsic_ext]
    "mk_intrinsic"
