(* Les expression *)
type expr =
  | Cst of valeur (*NB: Used to represent function in source too with a Cst(Fun(_))*)
  | Var of string
  | Call of expr * expr
  | Let of pattern * expr * expr * bool (*let string = expr in expr, recursive ?*)
  | Control_flow of control_flow
  | Constructor of string * expr list
  | Try of expr * ((pattern * expr) list)
  | Raise of expr
(*The constructor name, and eventual arguments. Arguments are only provided*)

(*when clear in the syntax (tupple, constructor ::), else Call(Constructor(name,[]),argument)*)
(*will be used instead (like a function application). Note that the identifier "ctx:"^name is used
                                       to store information relative to this constructor*)

(* les valeurs *)
and valeur =
  | VI of int
  | Construct of string * valeur list  (*the contructor name, then the arguments.  Boolean are there as Construct("true"/"false",[])*)

  | Fun of pattern * expr * ( (string*valeur) list option )
    (*Fun(binding,expression,capture list) is fun binding -> expression. The capture list is empty in the source code and filled when evaluation occur (closure) *)

  | Intrinsic of (valeur -> context -> valeur) * string
    (*A function defined outside of fouine (in ocaml). Used to implement things like print_int or (+) *)

  | Unit

(* Le contexte *)
(* Utilisé pour faire des association variable -> valeur, constructeur -> nombre d'arguments, et ref -> contenu *)
and context =
  { vars : (string, valeur) Hashtbl.t
  ; constructors : (string, int) Hashtbl.t
  ; refs : vec
  }

(*Represent every control flow (if, match, loop) as a tuple (decide,branch,loop) using the following logic:
  - on evalue decide
  - on prend la première branche ayant un pattern accetptant (avec les eventuels binding correspondant)
  - si loop vaut vrai, on recommence tant qu'on peut prendre une branche, puis on renvoie unit
  - sinon, si on a pris une branche, on renvoie la valeur obtenue, sinon on plante
  *)
and control_flow = expr * (pattern * expr) list * bool

(*match arm, let assignement and function argument*)
and pattern =
  | Binding of string
  | Exact of valeur (*Note: function are forbiden here*)
  | Constr_p of string * pattern list
  | Either of pattern * pattern

and vec =
  { (*Ocupied slot are the value directly*)
    (*Unocupied slot hold a VI(0)*)
    mutable store : valeur array
  ; (*index of a next free slot (or lenght of store if full)*)
    mutable next_slot : int
  }

module StringSet = Set.Make (String)

type stringset = StringSet.t

module IntSet = Set.Make (Int)

type intset = IntSet.t

exception SyntaxError of string (*User entered a program with a syntax error*)

exception
  InternalError of string (*Something have gone wrong in there. Not the user's fault*)

exception RuntimeError of string (*Something have gone wrong at runtime*)
