type identifier =
  | Void
  | Id of string
  | Intern_func_id (* faux identifiant utilisé pour les fonctions anonymes créées à la compilation *)

exception EnvironmentMissException of identifier

type 'a environment = (identifier * 'a) list

let empty_env = []

let add env id v =
  match id with
  | Id _
  | Intern_func_id  -> (id, v)::env
  | Void            -> env

(* Ajoute un identifiant s'il existe *)
(* pour les let rec, c'est si la fonction est récursive *)
let add_rec env id =
  add env (Option.value ~default:Void id)

let get env id =
  try
    List.assoc id env
  with
    Not_found -> raise (EnvironmentMissException id)

