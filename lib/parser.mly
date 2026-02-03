%{
(* --- PARTIE 1, préambule : ici du code Caml --- *)

open Expressions   (* rappel: dans expressions.ml: 
             type expr = Cst of int | Add of expr*expr | Mul of expr*expr | Min of expr*expr *)


%}

/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token LPAREN RPAREN PTVIRGULE EOF
%token IF THEN ELSE TRUE FALSE
%token LET IN FUN REC ARROW
%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> IDENT
%token ASSIGN
%token MINUS QUAD_DOT AND OR EQUAL
%token <string> OP_BANG OP_HASH OP_STARSTAR OP_STAR OP_PLUS OP_AT OP_EQ

/* PARTIE 3, on donne les associativités et on classe les priorités (- -> +)prio ******** */
%right PTVIRGULE
%right THEN ELSE
%nonassoc IN ARROW
%right ASSIGN
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

%left TRUE LPAREN FALSE IDENT INT

/* PARTIE 4, le point d'entrée ******************************************* */
		    
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expressions.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */


/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
| e=expression EOF { e }  /* on reconnaît une expression suivie de "EndOfFile", on la renvoie telle quelle */
  

/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */                                                                                
expression:
  | e=param                               { e }   
  | e1=expression o=operator e2=expression{ Call(Call(Var("( "^o^" )"),e1),e2) }
  | MINUS e=expression %prec UNARY_MINUS  { Call(Call(Var("( - )"),Cst(VI(0))),e) } (* le moins unaire *)
  | e1=expression PTVIRGULE e2=expression { Let("_",e1,e2,false) }
  | e1=expression PTVIRGULE               { e1 }
  | e1=expression e2=param                { Call(e1,e2) }
  | IF e1=expression THEN e2=expression ELSE e3=expression {
             Control_flow(e1,[
				(Exact (VB true),e2);
				(Exact (VB false),e3);
			],false) }
  | IF e1=expression THEN e2=expression   {
		Control_flow(e1,[
			(Exact (VB true),e2);
			(Exact (VB false),Cst(Unit));
		],false)}

  | LET rc=some_rec id=ident lst=identifier_list EQUAL e1=expression IN e2=expression
      {
         Let(id,Expressions.curify lst e1,e2,rc)
      }

  | FUN id=ident lst=identifier_list ARROW e1=expression         {
      Cst(Fun(Some id,
         Expressions.curify lst e1
      ))}

identifier_list:
   | { [] }
   | id=ident list=identifier_list { id::list }

ident:
  | id=IDENT { id }
  | LPAREN op=operator RPAREN { "( "^op^" )" }
  | LPAREN op=prefix_operator RPAREN { "( "^op^" )" }

param:
  | LPAREN e=expression RPAREN            { e }
  | LPAREN RPAREN                         { Cst(Unit) }
  | TRUE                                  { Cst (VB(true)) }
  | FALSE                                 { Cst (VB(false))}
  | id=ident                              { Var(id) }
  | i=INT                                 { Cst (VI i) }
  | op=prefix_operator e=param            { Call(Var("( "^op^" )"),e) }

%inline operator:
| ASSIGN         {":="}
| MINUS          {"-"}
| QUAD_DOT       {"::"}
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


some_rec:
| REC {true}
| (*empty*) {false}
