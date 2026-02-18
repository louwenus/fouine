(* Les expression *)
type expr =
  | Cst of valeur
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
  | Construct of string * valeur list
  (*the contructor name, then the arguments*)
  (*Note that we use the illegal identifier "ctr:"^name to store some information about that contructor*)
  | Fun of pattern * expr
    (*Fun(Some(binding),expression) is fun binding -> expression. The None form is fun () -> *)
  | Intrinsic of (valeur -> context -> valeur) * string
    (*A function defined outside of fouine (in ocaml). Used to implement things like print_int or (+) *)
  | Unit

(* Le contexte *)
(* Utilisé pour faire des association variable -> valeur *)
(* Utilisé également avec des identifiers illégaux tels que "type:nom" pour stocker de l'information a propos d'autres choses *)
and context =
  { vars : (string, valeur) Hashtbl.t
  ; constructors : (string, int) Hashtbl.t
  ; refs : sparse_vec
  }

(*Represent every? control flow as a tuple (decide,branch,loop) using the following logic:
  - on evalue decide
  - on prend la première branche ayant un pattern accetptant (avec les eventuels binding correspondant)
  - si loop vaut vrai, on recommence tant qu'on peut prendre une branche, puis on renvoie unit
  - sinon, si on a pris une branche, on renvoie la valeur obtenue, sinon on plante
  *)
and control_flow = expr * (pattern * expr) list * bool

(*Used mostly for match arm, but in the end will also be used for let and function arg*)
and pattern =
  | Binding of string
  | Exact of valeur (*Note: function are forbiden here*)
  | Constr_p of string * pattern list
  | Either of pattern * pattern
(*a constructor name then the sub pattern for each argument*)

and sparse_vec =
  { (*Ocupied slot are the value directly*)
    (*Unocupied slot hold a VI(x) where x is the index of the next free slot*)
    (*-1 represent no more empty slot*)
    mutable store : valeur array
  ; (*index of a free slot or -1 if no such slot exist*)
    mutable free_slot : int
  }

module StringSet = Set.Make (String)

type stringset = StringSet.t

module IntSet = Set.Make (Int)

type intset = IntSet.t

exception SyntaxError of string (*User entered a program with a syntax error*)

exception
  InternalError of string (*Something have gone wrong in there. Not the user's fault*)

exception RuntimeError of string (*Something have gone wrong at runtime*)
