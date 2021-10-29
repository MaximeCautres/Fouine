open Expr
open Exceptions
open TypeUtils

(**********************************)
(* définitions des types utilisés *)
(**********************************)

(* Une description consiste soit en un objet construit, soit un pointeur vers une référence de description *)
type descr =
  | Link of t
  | Repr of constr

(* un miroir du type TypeUtils.t, mais pour le partage *)
and constr =
  | Op of TypeUtils.op * (t list)
  | Var of var

and t = descr ref

(* comme les mêmes types dans TypeUtils... *)
type problem = (t * t) list
type subst = (var * t) list


(*************************************************************)
(* Transformation des types de TypeUtils en types du partage *)
(*************************************************************)

(* une table de hachage pour être efficace, de taille initiale arbitraire... *)
let vars : subst ref = ref []

let add v t =
  let rec add_to_l l =
    match l with
    | (v',_)::l' when v'=v  -> (v,t)::l'
    | (v',t')::l'           -> (v',t')::(add_to_l l')
    | []                    -> [(v,t)]
  in
  vars := add_to_l !vars

let ref_repr o l = ref (Repr (Op(o,l)))

let rec sharing_of_type (tipe : TypeUtils.t) : t =
  match tipe with
  | TOp(o,l) -> ref_repr o (List.map sharing_of_type l)
  | TVar v   ->
      try
        List.assoc v !vars
      with Not_found -> let r = ref (Repr (Var v)) in add v r; r

let sharing_of_problem (pb : TypeUtils.problem) : problem =
  List.map (fun (t,u) -> sharing_of_type t, sharing_of_type u) pb


(**************************************************)
(* Ajout des liens et raccourcicement des chemins *)
(**************************************************)

let rec repr (tipe : t) : t =
  match !tipe with
  | Repr c -> tipe
  | Link t' ->
      let r = repr t' in
      tipe:= Link r;
      r


(*******************************)
(* Convertit dans l'autre sens *)
(*******************************)

let rec unsharing_of_type (tipe : t) : TypeUtils.t =
  match !(repr tipe) with
  | Link _ -> assert false (* pas atteint, par def de `repr` *)
  | Repr (Var v) -> TVar v
  | Repr (Op(o,l)) -> TOp(o, List.map unsharing_of_type l)

let unsharing_of_subst (s : subst) : TypeUtils.subst =
  List.map (fun (v, t) -> (v, unsharing_of_type t)) s

let raise_unsharing (t1: t) (t2: t) =
  raise (NotUnifiableException (unsharing_of_type t1, unsharing_of_type t2))


(*********************)
(* Test de cyclicité *)
(*********************)

let cyclic t =
  let rec aux visited t =
    List.memq t visited
    ||
    begin
      match !(repr t) with
      | Repr (Op (op,l)) -> List.exists (aux (t::visited)) l
      | _                -> false
    end
  in aux [] t


(************************)
(* Enfin, l'unification *)
(************************)


let rec unify_sharing t1 t2 =
  if cyclic t1 || cyclic t2 then failwith "cyclique !";
  let r1, r2 = repr t1, repr t2 in
  match !r1, !r2 with
  | Link _, _ | _, Link _ -> assert false (* pas atteint, par def de `repr` *)
  | Repr (Var v), Repr (Var v') when v=v' -> ()
  | Repr (Var _), _ -> r1 := Link r2
  | _, Repr (Var _) -> r2 := Link r1
  | Repr Op(o1,l1), Repr Op(o2,l2) when o1=o2 -> begin
      try
        List.iter2 unify_sharing l1 l2
      with Invalid_argument _ -> raise_unsharing t1 t2
    end
  | Repr (Op _), Repr (Op _) -> raise_unsharing t1 t2

let unify_sharing_pb = List.iter (fun (t1,t2) -> unify_sharing t1 t2)

let unify (pb : TypeUtils.problem) : TypeUtils.subst =
  unify_sharing_pb (sharing_of_problem pb);
  unsharing_of_subst !vars

