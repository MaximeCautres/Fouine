Question plus sérieuse : Quand on ne traite que les expressions arithmétiques, à quoi sert `UMINUS` ?

Si j'ai bien compris, pour traiter `-2+-2`, lorsque l'automate arrive à l'état `-2.+-2` il y a un conflit entre :
- `reduce` le `MINUS` pour finalement renvoyer l'équivalent de `(-2)+(-2)`; et
- `shift` pour finalement renvoyer l'équivalent de `-(2+(-2))`

Or, la ligne `%left PLUS MINUS` permet que ce conflit shift/reduce soit automatiquement résolu en faveur du `reduce` [1].

Avec le `%prec UMINUS`, on donne la priorité à la règle qui contient le `MINUS`, donc au `reduce`, ce qui ne change pas du comportement sans `UMINUS` (et le résultat de mon exemple est bien `Add(Min(0, 2), Min(0, 2))` dans les deux cas)

[1]: <https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#ss:ocamlyacc-declarations>

Update:\
Oh! C'est un peu plus subtil en fait.\
J'ai compilé avec et sans `%prec UMINUS` et j'ai diff les deux `parser.output`.\
La seule chose qui change, c'est
- sans `UMINUS` :
  ```ocaml
    state 8
        expression : expression . PLUS expression  (4)
        expression : expression . TIMES expression  (5)
        expression : expression . MINUS expression  (6)
        expression : MINUS expression .  (7)

  !     TIMES  shift 11
  !     PLUS  reduce 7
  !     MINUS  reduce 7
  !     RPAREN  reduce 7
  !     EOL  reduce 7
  ```
- avec `UMINUS` :
  ```ocaml
    state 8
        expression : expression . PLUS expression  (4)
        expression : expression . TIMES expression  (5)
        expression : expression . MINUS expression  (6)
        expression : MINUS expression .  (7)

  !     .  reduce 7
  ```
Donc le `UMINUS` fait explicitement prendre la priorité au signe moins sur la multiplication : on réduit *toujours*
- sans `UMINUS`, on a `-2*-2` -> `Min(0, Mul(2, Min(0, 2)))`, ie `-(2*(-2))`
- avec `UMINUS`, on a `-2*-2` -> `Mul(Min(0, 2), Min(0, 2))`, ie `(-2)*(-2)`

Mais puisque la multiplication par -1 est commutative, on ne devrait pas voir de différence numérique. Pourriez-vous me le confirmer, ou dans le cas contraire fournir un exemple où les deux versions produisent un résultat numérique différent ?

