open Types
open Expressions

(*These function are used by the mk_intrinsic macro to cast a fouine value to the appropriate ocaml value*)
let to_int = function
  | VI x -> x
  | _ -> raise (SyntaxError "Unexpected argument: expected a int")
;;

let to_unit = function
  | Unit -> ()
  | _ -> raise (SyntaxError "Unexpected argument: expected a unit")
;;

let to_bool = function
  | Construct ("true", []) -> true
  | Construct ("false", []) -> false
  | _ -> raise (SyntaxError "Unexpected argument: expected a bool")
;;

(*These function are used by the mk_intrinsic macro to cast back an ocaml value into a fouine value*)
let from_int x = VI x
let from_unit (_ : unit) = Unit
let from_bool b = Construct (string_of_bool b, [])

(*This is used by mk_intrinsic to abstract the inner details of the Intrinsic*)
let partial
      (name : string)
      (to_ty : valeur -> 'a)
      (finalize : 'b -> valeur)
      (f : 'a -> 'b)
  =
  Intrinsic
    ( (fun x _ ->
        let f = f (to_ty x) in
        finalize f)
    , name )
;;

(*Some directly defined intrinsic*)
(*ref operator*)
let intr_ref =
  Intrinsic
    ( (fun v ctx ->
        let idx = ctx.refs.next_slot in
        let n = Array.length ctx.refs.store in
        if idx >= n then
          ctx.refs.store <- Array.append ctx.refs.store (Array.make n (VI 0));
        ctx.refs.next_slot <- idx + 1;
        ctx.refs.store.(idx) <- v;
        Construct ("ref", [ VI idx ]))
    , "ref" )
;;

(*:= operator*)
let intr_assign =
  Intrinsic
    ( (fun v _ ->
        match v with
        | Construct ("ref", [ VI idx ]) ->
          Intrinsic
            ( (fun v ctx ->
                ctx.refs.store.(idx) <- v;
                Unit)
            , "( := ) " ^ string_of_int idx )
        | _ -> raise (SyntaxError "Operator := used on not a ref"))
    , "( := )" )
;;

(*! operator*)
let intr_deref =
  Intrinsic
    ( (fun v ctx ->
        match v with
        | Construct ("ref", [ VI idx ]) -> ctx.refs.store.(idx)
        | _ -> raise (SyntaxError "Operator ! used on not a ref"))
    , "( ! )" )
;;

(*Now we call the macro (defined in ../mk_intrinsic/mk_intrinsic.ml) to automatically generate a bunch of intrinsic from the coressponding ocaml function*)
(*the list also contains the previous 3 intrinsics*)
let std_intrinsics =
  [%mk_intrinsic
    [ (( + ) : int -> int -> int)
    ; (( - ) : int -> int -> int)
    ; (( / ) : int -> int -> int)
    ; (( * ) : int -> int -> int)
    ; (( >= ) : int -> int -> bool)
    ; (( <= ) : int -> int -> bool)
    ; (( < ) : int -> int -> bool)
    ; (( > ) : int -> int -> bool)
    ; (( <> ) : int -> int -> bool)
    ; (( = ) : int -> int -> bool)
    ; (( && ) : bool -> bool -> bool)
    ; (( || ) : bool -> bool -> bool)
    ; (not : bool -> bool)
    ; (( mod ) : int -> int -> int)
    ; (( land ) : int -> int -> int)
    ; (( lor ) : int -> int -> int)
    ; (( lxor ) : int -> int -> int)
    ; (( lsl ) : int -> int -> int)
    ; (( lsr ) : int -> int -> int)
    ; (( asr ) : int -> int -> int)
    ; (print_int : int -> unit)
    ; (read_int : unit -> int)
    ; (print_newline : unit -> unit)
    ]]
  @ [ intr_ref; intr_deref; intr_assign ]
;;

let std_constructor =
  [ (*Std defined constructor and their arity*)

    (*bool*)
    "true", 0
  ; "false", 0
  ; (*list*)
    "[]", 0
  ; "(::)", 2
  ; (*Tuples have no name to be printed in adequate manner*)
    "", -1
  ; (*references*)
    "ref", 1
  ; (*Error defined by default*)
    "E", 1
  ]
;;

(*Parts of the std wich can be defined in fouine itself.
Note that order matter: subsequent function can use previous ones along with intrinsics*)
let std_in_lang =
  [ "prInt", "fun x -> (\n  print_int x;\n  print_newline ();\n  x)"
    (*for loops are hard to define later in case ! has been overwritten*)
  ; ( ":for_loop"
    , "fun start incr stop body ->\n\
      \                  let cnt = ref start in\n\
      \                  while !cnt * incr <= stop * incr do\n\
      \                    body !cnt;\n\
      \                    cnt := !cnt + incr\n\
      \                  done" )
  ]
;;

let uncaught _ = raise (RuntimeError "An exception was thrown and not caught!")

(*Construct the std by adding each things in order: the intrinsics, the constructors, and then the function defined in lang*)
let std =
  let std =
    { vars = Hashtbl.create 100
    ; constructors = Hashtbl.create 20
    ; refs = { store = Array.make 1 (VI (-1)); next_slot = 0 }
    }
  in
  List.iter
    (function
      | Intrinsic (f, name) -> add_var std name (Intrinsic (f, name))
      | _ -> raise (InternalError "an intrinsic was not an intrinsic"))
    std_intrinsics;
  List.iter (fun (name, arity) -> add_ctr std name arity) std_constructor;
  List.iter
    (fun (binding, definition) ->
       let lexbuf = Lexing.from_string definition in
       let e = Parser.main Lexer.token lexbuf in
       add_var std binding (eval_ctx std e Fun.id uncaught))
    std_in_lang;
  std
;;
