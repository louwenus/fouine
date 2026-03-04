open Lib
open Affichage

   (* "incantations" qu'il n'est pas nécessaire de comprendre dans un premier
   temps : on récupère l'entrée, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""

let recupere_entree () =
  let optlist = [
    ("-debug", Arg.Set verbose, "Active le mode de debuggage" );
    ("-shout", Arg.Set lettres_capitales, "Ecrit les operateurs en majuscules")
  ] in

  let usage = "Bienvenue a bord." in  (* message d'accueil, option -help *)

  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    optlist (* la liste des options definie plus haut *)

    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    usage; (* le message d'accueil *)
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
    affiche_expr e; (* on affiche e *)
    print_string " <-- affichage juste pour montrer l'arbre";
    print_newline();
    let v =  Eval.eval e in (* on évalue e *)
    print_int v;  (* on affiche le résultat *)
    print_newline();
  end

(* la fonction principale *)
let run () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e  (* <-- en cas d'exception *)


let _ = run ()
