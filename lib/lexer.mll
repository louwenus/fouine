{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+

(*Repris de la syntaxe ocaml directement https://ocaml.org/manual/5.4/lex.html#sss:lex-ops-symbols*)
let core_operator_char = ['$' '&' '*' '+' '-' '/' '=' '>' '@' '^' '|']
let op_char = ['~' '!' '?' '%' '<' ':' '.'] | core_operator_char

               
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']  { token lexbuf }    (* on saute les blancs et les tabulations *)
  | eof              { EOF }
  | "->"             { ARROW }
  | '='              { EQUAL }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | ';'              { PTVIRGULE }
  | "if"             { IF }
  | "then"           { THEN }
  | "else"           { ELSE }
  | "fun"            { FUN }
  | "rec"            { REC }
  | "true"           { TRUE }
  | "false"          { FALSE }
  | "let"            { LET }
  | "in"             { IN }
  | nombre as s      { INT (int_of_string s) }
  | "!="             { OP_EQ "!=" }
  | '-'              { MINUS }
  | ":="             { ASSIGN }
  | "::"             { QUAD_DOT }
  | "&&"             { AND }
  | '&'              { AND }
  | "or"             { OR }
  | "||"             { OR }
  | '!' op_char* as s{ OP_BANG s }
  | '~' op_char+ as s{ OP_BANG s }
  | '#' op_char+ as s{ OP_HASH s }
  | "**" op_char* as s{OP_STARSTAR s}
  | "lsl"            { OP_STARSTAR "lsl" }
  | "lsr"            { OP_STARSTAR "lsr" }
  | "asr"            { OP_STARSTAR "asr" }
  | ['*' '/' '%'] op_char* as s { OP_STAR s }
  | "mod"            { OP_STAR "mod" }
  | "land"            { OP_STAR "land" }
  | "lor"            { OP_STAR "lor" }
  | "lxor"            { OP_STAR "lxor" }
  | ['+' '-'] op_char* as s { OP_PLUS s }
  | ['@' '^'] op_char* as s { OP_AT s }
  | ['=' '<' '>' '|' '&' '$'] op_char* as s { OP_EQ s }
  | "(*"             { skip_comment 1 lexbuf }  (* skip and then continue *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']* as id { IDENT (id)}

and skip_comment n = parse
  | "(*" { skip_comment (n+1) lexbuf }
  | "*)" { if n = 1 then token lexbuf
            else skip_comment (n-1) lexbuf }
  | eof  { (* unterminated comment error *) raise End_of_file }
  | _    { skip_comment n lexbuf }
