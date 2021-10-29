Répartition du travail
======================
- Expression arithmétiques - Ensemble
- let..in - Pierre
- if then else - Maxime
- prInt - Pierre
- La factorisation du code des opérateurs - Maxime
- Les fonctions - Pierre
- Les aspects impératifs - Maxime
- Les listes - Pierre
- Les tuples - Ensemble
- Le matching - Maxime

- Faire les opérations comme l'énoncé veut - Maxime
- Les exceptions - Pierre
- La traduction CPS - Maxime
- Les traductions qui font mal au crâne - Ensemble

- Unification pour le typage - Pierre
- Creation du problème d'unification pour le typage - Maxime 

- L'autotest - Pierre
- Ptits trucs pour rendre le projet joli (Makefile, etc.) - Pierre
- Les merges - Git
- OCaml - Xavier Leroy


Documentation, explications
===========================
(Entre parenthèses, l'auteur de la section)

Le Makefile (Pierre)
--------------------
- L'exécutable s'appelle "fouine"
- `make` et `make clean` font ce qu'on attend
- `make remake` clean puis make, parce que `make clean && make` c'est trop long à taper...
- `make test` lance les tests
  + On peut préciser `TESTLEVEL=X` pour que les tests soient du niveau `X`, où `X∈{B,I,A}`
  + On peut préciser `TESTVERBOSE=` pour activer l'option `-v` de l'autotest
  + Par exemple `make TESTVERBOSE= TESTLEVEL=I` lance `autotest -v I`
- `make test_types` lance les tests du typage

La structure des expressions (Maxime)
-------------------------------------
(rédigé pendant qu'on faisait débutant, tout n'est pas à jour mais l'esprit est là)
Dans expr.ml, on a choisit un système de "type" qui nous permet d'être très flexible ce qui s’avérera très important dans la suite du projet.

Une expression est
- Une constante
- Une opération booléenne ou entière
- Un `let in`
- Un `if then else`
- Une variable
- Un `prInt`

Cela permet de partitionner les expression en de grandes catégories. Cela simplifie le traitement.

Avant ce travail de partitionnement, nous avions un code avec beaucoup de répétition et peu facilement adaptable.

Nous avons donc choisi de représenter d'une nouvelle manière les opérations. Par leur type (on ne refusera pas une addition entre un booléen et un entier lors de la compilation mais uniquement lors de l'évaluation, il ne s'agit donc pas d'un réel typage). Pour le moment nous avons 6 types d'opérations. Je vais expliquer le cas `Obaa`.

L'abréviation du constructeur `Obaa` correspond à l'*o*pération *b*inaire qui prend des *a*rith et renvoie des *a*rith. Les opérations de ce genre sont donc les opérations arithmétiques usuelles comme l'addition, la multiplication, la division et la soustraction.

Mémoire (Maxime)
----------------
La taille de la mémoire est fixe. On pourra la redimensionner dynamiquement un jour

let rec... (Pierre)
-------------------
Quand on parse un let rec, on regarde si ce qui vient après est une fonction, et le cas échéant un rajoute Some (son nom) dans la fonction pour qu'elle puisse faire référence à elle-même.

En Caml, on ne peut pas faire `let rec x = x in ...` parce que "This kind of expression is not allowed as right-hand side of `let rec'".
Notre fouine n'y voit pas particulièrement de problème, et ne renvoie pas d'erreur si `x` a été défini avant. cf le test `Intermediate/ManualOutput/rec_nofunc.ml`

Tuples (Pierre)
---------------
Ca a été galère pendant longtemps. Là je pense que ça marche en faisant tout à l'envers et avec List.rev à la fin. Comme ça on le force à aller lire les COMMA autant que possible avant de réduire.
NB: on est malin et on sait faire la différence entre 1,2,3 et 1,(2,3)

Point virgule moche (Pierre)
----------------------------
Les `;` à la fin des expressions (a;b;c;)... je trouve pas ça beau mais ça existe
C'est pas difficile à implémenter mais c'est un bout de syntaxe un peu rare donc je précise qu'on l'a implémenté

Trace (Pierre)
--------------
On a ajouté une option `-trace` qui affiche la trace de l'automate: c'est très pratique pour debugger le parser !

Rendu 3
=======
Ordre des opérations
--------------------
C'était pas bon donc on les refait.
A savoir que l'ordre n'est pas toujours de droit à gauche dans Caml pour l'application, à cause des optimisations. En fouine on force toujours de droite à gauche, donc il faut faire gaffe en écrivant des tests.

`eval` avec des continuations
-----------------------------
Les fonctions pour les opérateurs sont un peu moches et répétitives. Soit ces fonctions sont dans le grand (let rec...and...) qui constitue tout le fichier, soit il faut leur passer d'autres fonctions d'évaluation en argument. Je juge la deuxième option plus moche que de ne pas factoriser, donc on n'y touche plus.

Mémoire
-------
Le test pour les problèmes de mémoire est fait pour la borne hardcodée et arbitraire de 100 cases mémoire...

Raise
-----
La syntaxe `RAISE LPAREN E expr_atomique RPAREN` est figée. En effet, les exceptions n'étant pas des valeurs on ne peut avoir de `raise e` où `e` vaut `E 5`..., ou de `raise (print_string "je vais raise !\n"; E 5) comme en Caml
Par contre on peut faire `raise (E (1+1))`.

Tests
-----
Il y a quelques nouveaux tests pour les exceptions, qui sont dans Advanced.
Il y a aussi des tests qui correspondent aux retours du rendu2, ils sont aussi dans Advanced.

Makefile
--------
On a maintenant un `make test_cps` qui lance un `fouine -cps -autotest` sur tous les fichiers de test normaux (donc ni ShouldFail ni ManualOutput).

Problème de traduction
----------------------
Quand on traduit (à la main, en suivant `exceptions-continuations.pdf`) `let f = fun x -> x in (f 1; f true)`, on obtient quelque chose qui ressemble à `(fun f -> f 1; f true) (fun x -> x)`. Or cette expression ne compile pas en Caml car `f` est typée `int -> 'a` dès que le parser voit `f 1`, et plante en voyant le booléen...
Le test `app_prio.ml`, qui fait quelque chose comme ça, est le seul qui ne peut pas passer.

"jamais atteint"
----------------
Certains constructeurs ne sont jamais utilisés dans la traduction, puisqu'on a remplacé ces constructeurs par des fonctions. On a quand même écrit les traductions pour le fun, mais on ne peut pas vraiment savoir si elles sont correctes.
On notera que ces fonctions sont redéfinies dans `preludeCPS.ml` pour prendre des continuations.

Les environnement de typage
---------------------------
Pour régler le problème de collision de variable de typage (par exemple, X_x pour deux x différent dans le code), on utilise un environnement similaire à celui de l'évaluation. On a une pile des variable créée. Quand on créée une nouvelle variable on l'ajoute avec sa variable de type dans l'environnement, si on la cherche, on remonte jusqu'a trouver la première occurrence de celle-ci. Si on en trouve aucune, cela veut dire que de base la variable n'a pas été définie, on lui ajoute une variable de type pour que le problème saute et réapparaisse lors de l'évaluation, ce qui est bien le cas sur caml.   

Les tests du typage
-------------------
Il y a quelques tests spécifiques au typage, mais en réalité le fait que tout le reste passe (sauf ceux qui ont été construits pour ne pas passer en présence de types).
Deux tests dans "NO" passent en présence de polymorphisme, et ne génèrent pas d'erreur : c'est donc attendu.

