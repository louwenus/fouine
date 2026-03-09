# Fonctionnalitées actuelles de ma fouine:
Types entier, bool, fonction, unit, liste et couples fonctionnels (y compris les couples sans parenthèse)

Declarations, séquences, let top-level.

Fonction récurisves, y compris mutuellement récursives (syntaxe let rec ... and ... ou avec des tupple/liste (let rec [f;g;h] = [...])).

try et raise (gérée par eval en cps)

Une belle libraire standard (voir lib/std.ml). Elle définit:
  - Quelques fonction de la std ocaml, rendues disponible en fouine
  - Les opérateurs classiques (+,-,*,<,...)
  - quelques fonction suplémentaires telles que demandé (prInt)

La possibilité, comme en Ocaml, de définir/redéfinir nos opérateurs (let ( + ) a b = a + a + b in 10 + 10)
avec les noms de la std ou d'autres tant qu'ils utilisent les symboles d'opérateurs.
Comme en caml, priorité, associativité, infixe/prefixe déterminé par le 1er (ou les 2 premiers) char.

Commentaires (récursifs) délimités par (*  *)

Début de traduction CPS de la source (non finis, non testé, non lié au options. Voir traduction.ml)

Tests:
- ajout sur chaque test d'un comparaison fouine / ocaml executé sur le retour de fouine -showsrc (modification de autotest.sh)
- ajouts de quelques test méchants
- 1 test (closure avec variables non définies) passé en ShouldFail

# Caveats
- Un test qui était dans ManualOutput ne passe pas (et a donc été passé dans ShouldFail):
  definition d'une closure avec des variables libres non définies ->
  attendu: rien tant qu'on utilise pas la closure.
  mon comportement: crash au moment de définir la closure.

- Let rec (avec and eventuels) ne peut définir que des fonction, avec le premier argument visible. Tenter de définir autre chose risque de provoquer des erreurs.

- différence de comportement ocaml/fouine:
  + mes opérateurs && et || ne sont pas lazy
     utilisation de preludeCaml.ml pour retirer le comportement lazy d'ocaml sur les tests.
  + Mes commentaires sont plus permissifs dans leur contenu que ocaml (car je ne gère pas les strings)
  + Ordre d'évaluation: l'ordre d'évaluation des éléments d'un tupple (et peut être des listes aussi) n'est pas celui de ocaml actuel
    la doc Ocaml spécifie bien que c'est cencé être non-determiné: https://ocaml.org/manual/5.4/expr.html#ss:expr-ops-on-data
    Normalement le reste est bien conforme (arguments d'une fonction évalués avant celle ci notament.)
