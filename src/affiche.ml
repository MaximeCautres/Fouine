open Expr
open Env
open TypeUtils

(* affiche une liste de trucs séparés par sep *)
(* définie hors des `and` pour être polymorphe *)
let string_of_list f sep = List.fold_left (fun s x -> s ^ (if s="" then s else sep) ^ f x) ""


(*********************************************************************)
(* fonctions d'affichage utilisés principalement dans les exceptions *)
(*********************************************************************)

let name_of_val = function
  | Int _         -> "un entier"
  | Bool _        -> "un booléen"
  | Closure _     -> "une fonction"
  | Address _     -> "une adresse"
  | Unit          -> "un unit"
  | VTuple _      -> "un tuple"
  | VList _       -> "une liste"

let string_of_id = function
  | Void            -> "_"
  | Id s            -> s
  | Intern_func_id  -> "intern__" (* ce '?' est factice, il sert pour les `function` *)

let rec string_of_value = function
  | Int n               -> string_of_int n
  | Bool b              -> string_of_bool b
  | Closure (_,(m,e,r)) -> "(fun " ^ string_of_motif m ^ " -> " ^ (string_of_expr e) ^ ")"
  | Address a           -> string_of_int a
  | Unit                -> "()"
  | VTuple t            -> "(" ^ string_of_list string_of_value "," t ^ ")"
  | VList l             -> "[" ^ string_of_list string_of_value ";" l ^ "]"


(****************************************)
(* fonctions d'affichage des opérations *)
(****************************************)

and string_of_bin_op = function
  | Obaa op' -> string_of_obaa op'
  | Obpb op' -> string_of_obpb op'
  | Obbb op' -> string_of_obbb op'
  | Obab op' -> string_of_obab op'


and string_of_unr_op = function
  | Oubb op' -> string_of_oubb op'

and string_of_obaa = function
  | Add -> "+"
  | Mul -> "*"
  | Min -> "-"
  | Div -> "/"

and string_of_obpb = function
  | Equal -> "="
  | Neq   -> "<>"

and string_of_obab = function
  | Lt  -> "<"
  | Gt  -> ">"
  | Leq -> "<="
  | Geq -> ">="

and string_of_obbb = function
  | Land -> "&&"
  | Lor -> "||"

and string_of_oubb = function
  | Lnot -> "not"


(***********************************************************)
(* fonctions diverses pour les let rec, match, function... *)
(***********************************************************)

and string_of_motif = function
  | MConst v     -> string_of_value v
  | MId i        -> string_of_id i
  | MTuple(l)    -> "(" ^ string_of_list string_of_motif "," l ^ ")"
  | MCons(m1,m2) -> "(" ^ string_of_motif m1 ^ "::" ^ string_of_motif m2 ^ ")"
  | MNil         -> "[]"

and string_of_fleche (m,e) = string_of_motif m ^ " -> " ^ string_of_expr e

and rec_or_not = function
  | Fun(_,_,Some _) -> "rec "
  | _               -> ""

(* détecte et affiche proprement les `function` *)
and string_of_fun m e r =
  match m,e with
  | (MId Intern_func_id,Match(Var Intern_func_id,ml)) -> "(function " ^ string_of_list string_of_fleche " | " ml ^ ")"
  | _                                                 -> "(fun " ^ string_of_motif m ^ " -> " ^ (string_of_expr e) ^ ")"


(*******************************************************************)
(* finalement, la fonction d'affichage d'une expression quelconque *)
(*******************************************************************)

and string_of_expr = function
  | Const k             -> string_of_value k
  | Ref e               -> "(ref " ^ string_of_expr e ^ ")"
  | Deref e             -> "(!" ^ string_of_expr e ^ ")"
  | Assign(e1,e2)       -> "(" ^ string_of_expr e1 ^ ":=" ^ string_of_expr e2 ^ ")"
  | Oper_bin(op,e1,e2)  -> "(" ^ string_of_expr e1 ^ " " ^ string_of_bin_op op ^ " " ^ string_of_expr e2 ^")"
  | Oper_unr(op,e)      -> "(" ^ string_of_unr_op op ^ " " ^ string_of_expr e ^ ")"
  | Let(m,e1,e2)        -> "(let " ^ rec_or_not e1 ^ string_of_motif m ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^ ")"
  | Var id              -> string_of_id id
  | Ifte(e1,e2,e3)      -> "(if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3 ^ ")"
  | Print(e)            -> "(prInt " ^ string_of_expr e ^ ")"
  | Fun(m,e,r)          -> string_of_fun m e r
  | App(e1,e2)          -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Seq(e1,e2)          -> "(" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ ")"
  | Match(e,ml)         -> "(match " ^ string_of_expr e ^ " with " ^ string_of_list string_of_fleche " | " ml ^ ")"
  | Tuple t             -> "(" ^ string_of_list string_of_expr "," t ^ ")"
  | EmptyList           -> "[]"
  | Cons(e1, e2)        -> "(" ^ string_of_expr e1 ^ "::" ^ string_of_expr e2 ^ ")"
  | TryWith(e1,n,e2)    -> "(try " ^ string_of_expr e1 ^ " with E " ^ string_of_id n ^ " -> " ^ string_of_expr e2 ^ ")"
  | Raise e             -> "(raise (E " ^ string_of_expr e ^ "))"


(***********************************)
(* fonctions d'affichage des types *)
(***********************************)

let string_of_var (v,i) =
  match v with
  | None -> "T" ^ string_of_int i
  | Some id -> "T{" ^ string_of_id id ^ "," ^ string_of_int i ^ "}"

let name_of_named_var (v,_) =
  match v with
  | None -> assert false
  | Some id -> string_of_id id

let anon_var_nbr = ref 0
let anonymous_vars : (int * string) list ref = ref []
let typename_number = ref 0
let typename_letter = ref (int_of_char 'a')

let next_prime i =
  let l = String.make 1 (char_of_int !typename_letter) in
  let n = if !typename_number = 0 then "" else string_of_int !typename_number in
  let p = "'" ^ l ^ n in
  incr typename_letter;
  if !typename_letter > (int_of_char 'z') then (typename_letter := (int_of_char 'a');incr typename_number);
  anonymous_vars := (i,p)::!anonymous_vars;
  p

let get_typename i =
  try
    List.assoc i !anonymous_vars
  with Not_found -> begin
    next_prime i
  end

let name_of_anonymous (_,i) = get_typename i   


let rec string_of_type = function
  | TVar(c) -> name_of_anonymous c 
  | TOp(op, tlist) -> begin
    match op with
    |TInt -> "int"
    | TBool -> "bool"
    | TFleche -> let t1 = List.hd tlist in
      let t2 = List.hd (List.tl tlist) in
      "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
    | TTuple -> "(" ^ string_of_list string_of_type " * " tlist ^ ")"
    | TUnit -> "unit"
    | TList -> "(" ^ string_of_type (List.hd tlist) ^ " list)"
    | TRef -> "(" ^ string_of_type (List.hd tlist) ^ " ref)"
  end

let affiche_pb =
  let affiche_equation (t1,t2) = print_endline (string_of_type t1 ^ " ?= " ^ string_of_type t2) in
  List.iter affiche_equation

let affiche_sub =
  let affiche_subline (v,t) = print_endline (string_of_var v ^ " ↦ " ^ string_of_type t) in
  List.iter affiche_subline

let affiche_surface sub l tipe =
  let affiche_surfline v = print_endline (name_of_named_var v ^ " : " ^ string_of_type (List.assoc v sub)) in
  List.iter affiche_surfline l;
  print_endline ("- : " ^ string_of_type (List.assoc tipe sub))

