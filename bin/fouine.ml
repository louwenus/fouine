open Lib

(* "incantations" qu'il n'est pas nécessaire de comprendre dans un premier
   temps : on récupère l'entrée, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""
let showsrc = ref false;;
let debug = ref false;;

let recupere_entree () =
  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    [("-showsrc", Arg.Set showsrc, "Output debug information");
   ("-debug", Arg.Set debug, "Set output file name")] (* la liste des options, vide *)
    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    ""; (* le message d'accueil, qui est vide *)
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "problème de saisie\n"; raise e)


(* le traitement d'une expression en entrée *)   
let execute e =
  begin
    Expressions.debug := !debug;
    Expressions.eval_ctx Std.std e ignore Std.uncaught
  end


(* la fonction principale *)
let run () =
  try
      let saisie = recupere_entree () in
      match !showsrc,!debug with
      | false,false ->	execute saisie; flush stdout
      | true,false -> Printf.printf "%s\n" (Affichage.affiche_expr saisie)
      | false,true | true,true -> Printf.printf "%s\n" (Affichage.affiche_expr saisie); execute saisie;flush stdout; 
  with e -> raise e  (* <-- en cas d'exception *)


let _ = run ()

