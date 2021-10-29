open Expr
open Env
open Debug
open Exceptions


(**********************)
(* sucres syntaxiques *)
(**********************)

let ( --> ) i e = Fun(MId (Id i), e, None)
let ( <| ) e1 e2 = App(e1,e2)
let v i = Var(Id i)
let ( |-> ) (k, kE) e = Fun(MTuple([MId (Id k); MId (Id kE)]), e, None)
let ( * ) e1 e2 = Tuple [e1;e2]

let num_k = ref 0
let num_ke = ref 0

let get_next_k () =
  let nom = "k" ^ string_of_int !num_k in
  incr num_k ;
  nom

let get_next_kE () =
  let nom = "kE" ^ string_of_int !num_ke in
  incr num_ke ;
  nom


(**************************************)
(* Traduction des opérations binaires *)
(**************************************)
let
rec transform_oper_bin = function
  | Obbb op -> transform_obbb op
  | op      -> transform_meme_ordre_bin op

and transform_meme_ordre_bin op =
  fun (k, kE) e1 e2 ->
  (transform e2) <| ("v2" --> (
    (transform e1) <| ("v1" --> (
     k <| Oper_bin(op, v "v1", v "v2"))
    ) * kE)
   ) * kE

and transform_obbb op = fun (k, kE) e1 e2 -> match op with
  | Land -> (transform e1) <| ("v1" --> Ifte(v "v1", transform e2 <| k*kE, transform (Const (Bool false)) <| k*kE)) * kE
  | Lor  -> (transform e1) <| ("v1" --> Ifte(v "v1", transform (Const (Bool true)) <| k*kE, transform e2 <| k*kE)) * kE

and transform_oper_unr = function  (* jamais appelé *)
  | op -> transform_meme_ordre_unr op

and transform_meme_ordre_unr op = (* traduit depuis les fonctions *)
  fun (k, kE) e -> (
  (transform e) <| ((("v" -->
    (k <| (Oper_unr(op, v "v")))
  ) * kE))
  )


(********************************************)
(* Traduction de la plupart des expressions *)
(********************************************)

and transform_let (k, kE) m e1 e2 =
  (transform e1) <| (("v1" -->
    Let(m, v "v1", transform e2 <| (k * kE)))
  * kE)

and transform_print (k, kE) e = (* traduit depuis les fonctions *)
  (transform e) <| ("v" --> (k <| Print(v "v"))) * kE

and transform_ifte (k, kE) e1 e2 e3 =
  (transform e1) <| (("v1" -->
    Ifte(v "v1",
    (transform e2) <| (k * kE),
    (transform e3) <| (k * kE)))
  * kE )

and transform_fun (k,kE) i e r =
  match r with
  | Some f -> k <| Let(MId f, Fun(i, transform e, r), Var f)
  | None   -> k <| Fun(i, transform e, r)

and transform_app (k, kE) e1 e2 =
  (transform e2) <| (("v2" --> (
    (transform e1) <| (("v1" --> (
      ((v "v1") <| (v "v2")) <| (k*kE))
    ) * kE))
  ) * kE)

and transform_ref (k, kE) e =
  transform e <| (("v" -->
    (k <| Ref (v "v")))
  * kE)

and transform_seq (k, kE) e1 e2 =
  (transform e1) <| (Fun(MId Void, transform e2 <| k*kE, None))*kE

and transform_deref (k, kE) e =
  transform e <| (("v" --> (
    k <| Deref (v "v")))
  * kE)

and transform_assign (k, kE) e1 e2 =
  transform e2 <| ("v2" --> (
    transform e1 <| ("v1" --> (
      k <| Assign(v "v1", v "v2")))
    * kE))
  * kE

and transform_tuple (k,kE) el =
  let vi i = "v" ^ (string_of_int i) in
  let rec aux el i =
    match el with
    | e::el' -> (transform e) <| Tuple [((vi i) --> (aux el' (i+1))); kE]
    | []     -> k <| Tuple (List.init i (fun j -> v (vi (i-j-1))))
  in
  aux (List.rev el) 0


and transform_match (k, kE) e l =
  transform e <| ("v" -->
    Match(v "v", List.map (fun (m, e) -> (m, transform e <| k*kE)) l))
  * kE

and transform_cons (k,kE) e1 e2 =
  (transform e2) <| (("t" --> (
    (transform e1) <| (("h" --> (
      k <| Cons(v "h", v "t")
    )) * kE)
  )) * kE)

and transform_trywith (k,kE) e1 i e2 =
    (transform e1) <| (k * ("n" --> Let(MId i, v "n", (transform e2) <| (k*kE))))

and transform_raise (k,kE) e =
  (transform e) <| ("v" --> (kE <| (v "v"))) * kE


(***************************************)
(* Finalement, la fonction transform ! *)
(***************************************)

and transform e =
  let k = get_next_k () in
  let kE = get_next_kE() in
  let k_couple = (k, kE) in
  let k_couple_v = (v k, v kE) in
  let e' = match e with
  | Const c             -> v k <| e
  | Var id              -> v k <| e
  | Oper_bin(op,e1,e2)  -> transform_oper_bin op k_couple_v e1 e2
  | Oper_unr(op, e)     -> transform_oper_unr op k_couple_v e (* jamais atteint *)
  | Print e             -> transform_print k_couple_v e (* jamais atteint *)
  | Let(m,e1,e2)        -> transform_let k_couple_v m e1 e2
  | Ifte(e1,e2,e3)      -> transform_ifte k_couple_v e1 e2 e3
  | Fun(i,e,r)          -> transform_fun k_couple_v i e r
  | App(e1,e2)          -> transform_app k_couple_v e1 e2
  | Seq(e1,e2)          -> transform_seq k_couple_v e1 e2
  | Ref e               -> transform_ref k_couple_v e (* jamais atteint *)
  | Deref e             -> transform_deref k_couple_v e
  | Assign(e1,e2)       -> transform_assign k_couple_v e1 e2
  | Tuple el            -> transform_tuple k_couple_v el
  | Match(e,l)          -> transform_match k_couple_v e l
  | EmptyList           -> v k <| EmptyList
  | Cons(e1,e2)         -> transform_cons k_couple_v e1 e2
  | TryWith(e1,i,e2)    -> transform_trywith k_couple_v e1 i e2
  | Raise e             -> transform_raise k_couple_v e
  in k_couple |-> e'

