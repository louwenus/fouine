# État actuel de ma fouine:
Types entier, bool, fonction et unit

Une belle libraire standard (voir lib/std.ml). Elle définit:
  - Quelques fonction de la std ocaml, rendues disponible en fouine
  - Les opérateurs classiques (+,-,*,<,...)
  - quelques fonction suplémentaires telles que demandé (prInt)

Declaration et fonction récursives
La possibilité, comme en Ocaml, de définir/redéfinir nos opérateurs (let ( + ) a b = a + a + b in 10 + 10)

; pour séparer des staments executé un a un
Retours a la lignes qui se comportent comme des espaces

Commentaires

# Caveats
- dune test peut planter sur le test si j'y ait laissé un read_int (dune ne permet pas d'entrée sur stdint)

- différence de comportement ocaml/fouine:
  + mes opérateurs && et || ne sont pas lazy
     (il se trouve que c'est relativement pénible a supporter quand && peut aussi être redéfini ou vu comme une fonction usuelle)
  + Mes commentaires sont plus permissifs dans leur contenu que ocaml
