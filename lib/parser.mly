%{
(* --- PARTIE 1, préambule : ici du code Caml --- *)

open Types
open Parser_helper
%}

/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token LPAREN RPAREN PTVIRGULE EOF LBRACKET RBRACKET PIPE
%token IF THEN ELSE TRUE FALSE MATCH WITH FOR WHILE DO DONE TO DOWNTO BEGIN END FUNCTION
%token TRY RAISE
%token LET IN FUN REC ARROW QUAD_VIRGULE
%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> IDENT CAPIDENT
%token ASSIGN
%token MINUS QUAD_DOT AND OR EQUAL COMMA
%token <string> OP_BANG OP_HASH OP_STARSTAR OP_STAR OP_PLUS OP_AT OP_EQ

/* PARTIE 3, on donne les associativités et on classe les priorités (- -> +)prio ******** */

%nonassoc GREEDYPIPE
%left PIPE

%right SEQUENCE
%right PTVIRGULE
%right THEN ELSE
%right ASSIGN
%nonassoc GREEDYCOMMA
%left COMMA
%right OR
%right AND
%left OP_EQ EQUAL
%right OP_AT
%right QUAD_DOT
%left OP_PLUS MINUS
%left OP_STAR
%right OP_STARSTAR
%nonassoc UNARY_MINUS
%left OP_HASH
%nonassoc OP_BANG 

%left TRUE LPAREN FALSE IDENT INT LBRACKET CAPIDENT BEGIN

/* PARTIE 4, le point d'entrée ******************************************* */
		    
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Types.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */

 
/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
| e=expression EOF { e }  /* on reconnaît une expression suivie de "EndOfFile", on la renvoie telle quelle */
| LET rc=boption(REC) p=pattern lst=pattern_list EQUAL e1=expression QUAD_VIRGULE e2=main
      {
         Let(p,curify lst e1,e2,rc)
      }

/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */                                                                                
expression:
  | e=exp_no_s %prec SEQUENCE                                {e}
  | e1=exp_no_s PTVIRGULE e2=expression                      { Let(Binding "_",e1,e2,false) }
  | e1=exp_no_s PTVIRGULE                                    { e1 }

exp_no_s:
  | t = tupple                    { match t with
                                    | [e] -> e
                                    | l -> Constructor("",l) }
  | e=exp_no_s ASSIGN e2=exp_no_s { Call(Call(Var("( := )"),e),e2) }

exp_no_s2:
  | e=param                                            { e }   
  | e1=exp_no_s2 o=operator_noassign e2=exp_no_s2      { Call(Call(Var("( "^o^" )"),e1),e2) }
  | MINUS e=exp_no_s2 %prec UNARY_MINUS                { Call(Call(Var("( - )"),Cst(VI(0))),e) } (* le moins unaire *)
  | e1=exp_no_s2 e2=param                              { Call(e1,e2) }
  | e1=exp_no_s2 QUAD_DOT e2=exp_no_s                  { Constructor("(::)",[e1;e2]) }
  | IF e1=exp_no_s THEN e2=exp_no_s ELSE e3=exp_no_s {
             Control_flow(e1,[
				(Constr_p("true",[]),e2);
				(Constr_p("false",[]),e3);
			],false) }
  | IF e1=exp_no_s THEN e2=exp_no_s   {
			Control_flow(e1,[
				(Constr_p("true",[]),e2);
				(Constr_p("false",[]),Cst(Unit));
			],false)}

  | LET rc=boption(REC) p=pattern lst=pattern_list EQUAL e1=expression IN e2=expression
      {
         Let(p,curify lst e1,e2,rc)
      }

  | FUN p=pattern lst=pattern_list ARROW e1=expression         {
      Cst(Fun(p,
         curify lst e1
      ))}

  | MATCH e=expression WITH lst=match_arms { Control_flow(e,lst,false) }
  | FUNCTION lst=match_arms                { Cst(Fun(Binding(":function"),Control_flow(Var(":function"),lst,false))) }
  | TRY e=expression WITH lst=match_arms   { Try(e,lst) }
  | RAISE p=param                          { Raise(p) }
  | WHILE cond=expression DO body=expression DONE { Control_flow(cond,[Constr_p("true",[]),body],true) }
  | FOR id=ident EQUAL start=expression incr=to_downto stop=expression DO body = expression DONE { for_loop id start incr stop body } 

tupple:
  | e=exp_no_s2 %prec GREEDYCOMMA { [e] }
  | e=exp_no_s2 COMMA lst=tupple { e::lst }


pattern_list:
   | { [] }
   | p=pattern list=pattern_list { p::list }

ident:
  | id=IDENT { id }
  | LPAREN op=operator RPAREN { "( "^op^" )" }
  | LPAREN op=prefix_operator RPAREN { "( "^op^" )" }

param:
  | LPAREN e=expression RPAREN            { e }
  | id=ident                              { Var(id) }
  | id=constructor                        { Constructor(id,[]) }
  | op=prefix_operator e=param            { Call(Var("( "^op^" )"),e) }
  | c=constant                            { Cst(c) }
  | LBRACKET lst=separated_list(PTVIRGULE, exp_no_s) RBRACKET{ list_expr lst } 
  | BEGIN e=expression END                { e }

%inline operator:
| ASSIGN         {":="}
| op=operator_noassign {op}

%inline operator_noassign:
| MINUS          {"-"}
| id=OP_HASH     {id}
| id=OP_STARSTAR {id}
| id=OP_STAR     {id}
| id=OP_PLUS     {id}
| id=OP_AT       {id}
| id=OP_EQ       {id}
| EQUAL          {"="}
| OR             { "||" }
| AND            { "&&" }

%inline prefix_operator:
| id=OP_BANG {id}

	

constant:
  | LPAREN RPAREN                         { Unit }
  | TRUE                                  { Construct("true",[]) }
  | FALSE                                 { Construct("false",[]) }
  | i=INT                                 { VI i }

match_arms:
  | PIPE? lst=delimited_match_arms { lst }
  
delimited_match_arms:
  | a=match_arm %prec GREEDYPIPE { [a] }
  | a=match_arm PIPE l=delimited_match_arms { a::l }
  

match_arm:
  | pat=pattern ARROW e=expression { (pat,e) }

pattern:
  | lst = separated_nonempty_list(COMMA,pat_no_coma) {
    match lst with
    | [p] -> p
    | l -> Constr_p("",l)
  }

(*  *)
pat_no_coma:
	| id = ident                                                  { Binding(id) }
    | p1=pat_no_coma PIPE p2=pat_no_coma                          { Either(p1,p2) }
	| c=constant                                                  { Exact(c) }
	| p1=pat_no_coma QUAD_DOT p2=pat_no_coma                      { Constr_p("(::)",[p1;p2]) }
    | LBRACKET lst=separated_list(PTVIRGULE, pattern) RBRACKET    { list_patt lst }
    | LPAREN p=pattern RPAREN                                     { p }
    | id=constructor                %prec OP_STAR                 { Constr_p(id,[]) }
    | id=constructor p=pat_no_coma  %prec OP_PLUS                 { match p with
                                                                    | Constr_p("",l) -> Constr_p(id,l)
                                                                    | p -> Constr_p(id,[p]) }

constructor:
  | id=CAPIDENT            { id }
  | LPAREN QUAD_DOT RPAREN { "(::)" }

to_downto:
  | TO { 1 }
  | DOWNTO { -1 }
