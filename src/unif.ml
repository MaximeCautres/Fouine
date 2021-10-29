open Expr
open Inference
open Exceptions
open TypeUtils

(*Renvoie true si x apparaît dans term*)
let rec appear (x : var) (term : t) : bool =
  match term with
  | TVar y          -> y=x
  | TOp(op,l)       -> List.exists (appear x) l

(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace ((x, new_x) as c: var * t) (term : t) : t =
  match term with
  | TVar y    -> if y=x then new_x else term
  | TOp(op,l) -> TOp(op,List.map (replace c) l)

let replace_pb x term = List.map (fun (t,u) -> replace (x,term) t, replace (x,term) u)

(*Implémente l'unification de deux termes*)
let rec unify (pb : problem) : subst =
  match pb with
  | [] -> []
  | (t,t')::tpb -> begin
    match t,t' with
    (* 1 *)
    | TOp(op1,l1), TOp(op2,l2) when op1=op2 -> begin
        try
          unify ((List.combine l1 l2) @ tpb)
        with Invalid_argument _ -> raise (NotUnifiableException (t,t'))
      end

    (* 3 *)
    | TVar x, TVar x' when x=x' -> unify tpb

    (* 4 *)
    | term, TVar x when appear x term -> raise (NotUnifiableException (t,t'))
    | TVar x, term when appear x term -> raise (NotUnifiableException (t,t'))

    (* 5 *)
    | term, TVar x
    | TVar x, term ->
        let tpb' = replace_pb x term tpb in
        let sub = unify tpb' in (x,term)::sub

    (* 2 et tout le reste *)
    | TOp _, TOp _ -> raise (NotUnifiableException (t,t'))
  end

