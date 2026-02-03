open Expressions



(*These function are used by the mk_intrinsic macro to cast a fouine value to the appropriate ocaml value*)
let to_int = function
| VI x -> x
| _ -> raise (Expressions.SyntaxError "Unexpected argument: expected a int") ;;

let to_unit = function
| Unit -> ()
| _ -> raise (Expressions.SyntaxError "Unexpected argument: expected a unit") ;;

let to_bool = function
| VB b -> b
| _ -> raise (Expressions.SyntaxError "Unexpected argument: expected a bool")

(*These function are used by the mk_intrinsic macro to cast back an ocaml value into a fouine value*)
let from_int x = VI x
let from_unit (_:unit) = Unit
let from_bool b = VB b

(*This is used by mk_intrinsic to abstract the inner details of the Intrinsic*)
let partial (name:string) (to_ty: Expressions.valeur -> 'a) (finalize: 'b -> valeur) (f: 'a -> 'b) =
Intrinsic(
    (fun x _ ->
        let f = f (to_ty x) in
        finalize f
    ),
    name
);;


(*This is a demonstration of the kind of arboresent Intrinsic we get to define something like (+) *)
let _ =
    let f = (+) in
    Intrinsic(
        (fun x _ ->
            let f = f (to_int x) in
            Intrinsic ((
                fun x _ ->
                let i = to_int x in
                from_int ( f i )
            ),"( + )")
        ),"( + )"
    );;

(*This is what the mk_intrinsic macro will actually write*)
let _ = let f = (+) in
partial "( + )" to_int (
    partial "( + )" to_int from_int
) f;;

(*Now we call the macro (defined in ../mk_intrinsic/mk_intrinsic.ml) to automatically generate a bunch of intrinsic from the coressponding ocaml function*)
let std_intrinsics = [%mk_intrinsic [
        (( + ):int->int->int);
        (( - ):int->int->int);
        (( / ):int->int->int);
        (( * ):int->int->int);
        (( >= ):int->int->bool);
        (( <= ):int->int->bool);
        (( < ):int->int->bool);
        (( > ):int->int->bool);
        (( != ):int->int->bool);
        (( = ):int->int->bool);
        (( && ):bool->bool->bool);
        (( || ):bool->bool->bool);
        ((mod):int->int->int);
        ((land):int->int->int);
        ((lor):int->int->int);
        ((lxor):int->int->int);
        ((lsl):int->int->int);
        ((lsr):int->int->int);
        ((asr):int->int->int);
        (print_int:int->unit);
        (read_int:unit->int);
        (print_newline:unit->unit);
    ]];;

(*Parts of the std wich can be defined in fouine itself.
Note that order matter: subsequent function can use previous ones along with intrinsics*)
let std_in_lang = [
        ("prInt","fun x -> (print_int x;x)")
    ]


let std =
  let std = Hashtbl.create 5 in
  List.iter (function
    (Intrinsic(f,name)) -> Hashtbl.add std name (Intrinsic(f,name))
    | _ -> raise (Expressions.InternalError "an intrinsic was not an intrinsic"))
    std_intrinsics;
  List.iter (
      fun (binding,definition) ->
      let lexbuf = Lexing.from_string definition in
      let e = Parser.main Lexer.token lexbuf in
      Hashtbl.add std binding (eval_ctx e std);
  )
  std_in_lang;
  std
;;
