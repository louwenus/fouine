open Expr
open Affichage


(* sémantique opérationnelle à grands pas *)
(* les appels à p_verb sont artificiels, ils illustrent l'usage des options de fouine *)
let rec eval = function
  | Const k -> k
  | Add(e1,e2) -> (p_verb "oula, une addition"; eval e1 + eval e2)
  | Mul(e1,e2) -> (p_verb "oula, une multiplication"; eval e1 * eval e2)
