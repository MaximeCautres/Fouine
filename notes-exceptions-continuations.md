# Exceptions

(E n) est-elle 
1. un expression atomique -> l'eval doit voir quand c'est pas une expression
2. un truc spécial -> erreur de syntaxe si c'est pas une exception

Je dirais 1. pour que ça soit plus souple dans le futur, mais on dirait que H veut que ce soit 2.


# Continuations

Slide BST avec continuation "break" pour remplacer une exception

```
[e1 e2] = fun k -> [e2] (fun v2 -> [e1] (fun v1 -> v1 v2 k))
[let x e1 in e2] = fun k -> [e1] (fun x -> [e2] k)
```
