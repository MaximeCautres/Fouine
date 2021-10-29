open Expr

(*****************************)
(* L'environnement de typage *)
(*****************************)

(* On va typer en suivant l'ordre de l'évaluation, on a donc
une mémoire des variables de type déja attribué pour éviter
les collisions *)

let i = ref 0

let get_next_var () =
  incr i;
  (None, !i)

let get_next_named_var id =
  let (_,v) = get_next_var () in
  (Some id, v)

type op = 
  | TInt
  | TBool
  | TUnit
  | TFleche
  | TTuple
  | TList
  | TRef
  
type var = Env.identifier option * int

type t = 
  | TVar of var 
  | TOp of op * (t list)

type schema = (var list) * t

type problem = (t * t) list
type subst = (var * t) list

type env = (var * schema) list

let add env id t = 
    match id with
    | Env.Id _ 
    | Env.Intern_func_id -> begin 
      match t with
      | Some t' -> (get_next_named_var id, t')::env
      | None    -> let variable =  get_next_named_var id in (variable, ([], TVar variable))::env
    end
    | Env.Void           -> env

let rec get env id = 
    match env with
    | [] -> let variable = get_next_named_var id in (variable, ([], TVar variable))
    | ((a, b), c) :: env' when a = Some id -> ((a, b), c)
    | e::env' -> get env' id
    
(*************************)
(* Le problème de Typage *)
(*************************)


(* On utilise une pile globale pour représenter le problème *)

let p : problem ref = ref []

let p_append t = p := t::!p

(* On suit la profondeur à laquelle on est pour détecter les let in de surface *)
let profondeur = ref 0
let id_surface : var list ref = ref []

let ajoute_surface v =
  if !profondeur = 0 then id_surface := v::!id_surface

