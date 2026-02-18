{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let letters = ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']

(*Repris de la syntaxe ocaml directement https://ocaml.org/manual/5.4/lex.html#sss:lex-ops-symbols*)
let core_operator_char = ['$' '&' '*' '+' '-' '/' '=' '>' '@' '^' '|']
let op_char = ['~' '!' '?' '%' '<' ':' '.'] | core_operator_char

               
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']  { token lexbuf }    (* on saute les blancs et les tabulations *)
  | eof              { EOF }
  | "->"             { ARROW }
  | '|'              { PIPE }
  | '='              { EQUAL }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '['              { LBRACKET }
  | ']'              { RBRACKET }
  | ';'              { PTVIRGULE }
  | ";;"             { QUAD_VIRGULE }
  | ','              { COMMA }
  | "if"             { IF }
  | "then"           { THEN }
  | "else"           { ELSE }
  | "fun"            { FUN }
  | "rec"            { REC }
  | "true"           { TRUE }
  | "false"          { FALSE }
  | "let"            { LET }
  | "in"             { IN }
  | "match"          { MATCH }
  | "with"           { WITH }
  | "for"            { FOR }
  | "while"          { WHILE }
  | "do"             { DO }
  | "done"           { DONE }
  | "to"             { TO }
  | "downto"         { DOWNTO }
  | "begin"          { BEGIN }
  | "end"            { END }
  | "try"            { TRY }
  | "raise"          { RAISE }
  | "function"       { FUNCTION }
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
  | ['*' '/' '%'] op_char* as s             { OP_STAR s }
  | "mod"                                   { OP_STAR "mod" }
  | "land"                                  { OP_STAR "land" }
  | "lor"                                   { OP_STAR "lor" }
  | "lxor"                                  { OP_STAR "lxor" }
  | ['+' '-'] op_char* as s                 { OP_PLUS s }
  | ['@' '^'] op_char* as s                 { OP_AT s }
  | ['=' '<' '>' '|' '&' '$'] op_char* as s { OP_EQ s }
  | "(*"                                    { skip_comment lexbuf; token lexbuf }  (* skip and then continue *)
  | ['a'-'z' '_'] letters* as id            { IDENT (id)}
  | ['A'-'Z'] letters* as id                { CAPIDENT (id) }

and skip_comment = parse
  | "(*" { skip_comment lexbuf;skip_comment lexbuf; }
  | "*)" { () }
  | eof  { (* unterminated comment error *) raise End_of_file; }
  | _    { skip_comment lexbuf; }

(*
and string = parse
  | [^'\\' '"']* as s {s}

and escape_sequence = parse
  | "\\\\" { '\\' }
  | "\\\"" { '"' }
  | "\\'"  { '\'' }
  | "\\n"  { '\n' }
  | "\\r"  { '\r' }
  | "\\t"  { '\t' }
  | "\\b"  { '\b' }
  | "\\ "  { '\ ' }
  | "\\i" ['0'-'9']['0'-'9']['0'-'9'] as s { (Scanf.unescaped s).[0] }
  | "\\x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'] as s { (Scanf.unescaped s).[0] }
  | "\\" ['0'-'7']['0'-'7']['0'-'7']['0'-'7'] as s { (Scanf.unescaped s).[0] }

and raw_string terminator =
  | 
*)
