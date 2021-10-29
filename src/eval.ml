open Expr
open Expr
open Env
open Debug
open Exceptions
open Affiche


let
(********************************************************)
(* fonctions d'évaluation des expressions arithmétiques *)
(********************************************************)

rec safe_int f = function
  | Int n -> f n
  | v     -> raise (ValueMismatchException.expInt v)

and safe_bool f = function
  | Bool n  -> f n
  | v       -> raise (ValueMismatchException.expBool v)

(* OBAA *)
and int_op_to_int f env e1 e2 (k,kE) =
  let k1 n2 = safe_int (fun n1 -> k (f n1 n2)) in
  let k2    = safe_int (fun n2 -> (eval_env env e1 (k1 n2,kE))) in
  eval_env env e2 (k2,kE)

and obaa_to_fun = function
  | Add -> ( + )
  | Mul -> ( * )
  | Min -> ( - )
  | Div -> ( / )

and eval_obaa o env e1 e2 (k,kE) =
  int_op_to_int (obaa_to_fun o) env e1 e2 ((fun n -> k (Int n)),kE)

(* OBAB *)
and int_op_to_bool f env e1 e2 (k,kE) =
  let k1 n2 = safe_int (fun n1 -> k (f n1 n2)) in
  let k2    = safe_int (fun n2 -> (eval_env env e1 (k1 n2,kE))) in
  eval_env env e2 (k2,kE)

and obab_to_fun = function
  | Lt  -> ( < )
  | Gt  -> ( > )
  | Geq -> ( >= )
  | Leq -> ( <= )

and eval_obab o env e1 e2 (k,kE) =
  int_op_to_bool (obab_to_fun o) env e1 e2 ((fun b -> k (Bool b)),kE)

(* OBPB *)
and p_op_to_bool f env e1 e2 (k,kE) =
  let k1 v2 v1 = k (f v1 v2) in
  let k2 v2    = eval_env env e1 (k1 v2,kE) in
  eval_env env e2 (k2,kE)

and obpb_to_fun = function
  | Equal -> ( = )
  | Neq   -> ( <> )

and eval_obpb o env e1 e2 (k,kE) =
  p_op_to_bool (obpb_to_fun o) env e1 e2 ((fun b -> k (Bool b)),kE)

(* OBBB *)
and eval_land env e1 e2 (k,kE) =
  let k2 = safe_bool k in
  let k1 = safe_bool (fun b1 -> if not b1 then k false else eval_env env e2 (k2,kE)) in
  eval_env env e1 (k1,kE)

and eval_lor env e1 e2 (k,kE) =
  let k2 = safe_bool k in
  let k1 = safe_bool (fun b1 -> if b1 then k true else eval_env env e2 (k2,kE)) in
  eval_env env e1 (k1,kE)

and eval_obbb o env e1 e2 (k,kE) = match o with
  | Land -> eval_land env e1 e2 ((fun b -> k (Bool b)),kE)
  | Lor  -> eval_lor env e1 e2 ((fun b -> k (Bool b)),kE)

(* OUBB *)
and oubb_to_fun = function
  | Lnot -> not

and eval_oubb o env e (k,kE) =
  let k1 = safe_bool (fun b -> k (Bool (oubb_to_fun o b))) in
  eval_env env e (k1,kE)


(**********************************)
(* utilitaire de pattern matching *)
(**********************************)

(*
 * ajoute les bons bindings à l'environnement en cas de match
 * renvoie None sinon
 *)
and find_match m v env =
  let match_list m ml v vl =
    Option.bind (find_match m v env) (find_match ml vl)
  in
  match m,v with
  | MConst(Int n),  Int n'  when n=n' -> Some env
  | MConst(Bool b), Bool b' when b=b' -> Some env
  | MConst(Unit),   Unit              -> Some env
  | MTuple [],      VTuple []         -> Some env
  | MId id,         _                 -> Some (Env.add env id v)
  | MTuple(m'::ml), VTuple(v'::vl)    -> match_list m' (MTuple ml) v' (VTuple vl)
  | MCons(m1,m2),   VList(v'::vl)     -> match_list m1 m2 v' (VList vl)
  | MNil,           VList []          -> Some env
  | _                                 -> None


(*************************************************************)
(* fonctions d'évaluation de certaines expressions complexes *)
(*************************************************************)

and eval_operation_bin = function
  | Obaa op' -> eval_obaa op'
  | Obab op' -> eval_obab op'
  | Obpb op' -> eval_obpb op'
  | Obbb op' -> eval_obbb op'

and eval_operation_unr = function
  | Oubb op' -> eval_oubb op'

and eval_let env m e1 e2 (k,kE) =
  let k1 v1 =
    match find_match m v1 env with
    | Some env' -> eval_env env' e2 (k,kE)
    | None      -> raise PatternMismatchException
  in
  eval_env env e1 (k1,kE)

and eval_ifte env e1 e2 e3 (k,kE) =
  let k1 = function
    | Bool true   -> eval_env env e2 (k,kE)
    | Bool false  -> eval_env env e3 (k,kE)
    | v           -> raise (ValueMismatchException.expBool v)
  in
  eval_env env e1 (k1,kE)

and eval_print env e (k,kE) =
  let k1 = safe_int (fun n -> print_int_debug n; k (Int n)) in
  eval_env env e (k1,kE)

and eval_app env e1 e2 (k,kE) =
  let k1 v f =
    match f with
    | Closure(env_f, (m,e_f,r)) -> begin
      match find_match m v env_f with
      | Some env_m -> eval_env (Env.add_rec env_m r f) e_f (k,kE)
      | None       -> raise PatternMismatchException
    end
    | v                         -> raise (ValueMismatchException.expClosure v)
  in

  let k2 v = eval_env env e1 (k1 v,kE) in
  eval_env env e2 (k2,kE)

and eval_ref env e (k,kE) =
  let k1 v = k (Address (Mem.add v)) in
  eval_env env e (k1,kE)

and eval_deref env e (k,kE) =
  let k1 = function
    | Address a -> k (Mem.get a)
    | v         -> raise (ValueMismatchException.expAddress v)
  in
  eval_env env e (k1,kE)

and eval_assign env e1 e2 (k,kE) =
  let k1 v = function
    | Address a -> Mem.set a v; k Unit
    | v         -> raise (ValueMismatchException.expAddress v)
  in
  let k2 v = eval_env env e1 (k1 v,kE) in
  eval_env env e2 (k2,kE)

and eval_tuple env e (k,kE) =
  let add_head vt v =
    match vt with
    | VTuple t -> k (VTuple (v::t))
    | _        -> assert false (* n'arrive pas *)
  in
  let eval_head e vt = eval_env env e (add_head vt,kE) in
  match e with
  | Tuple []     -> k (VTuple [])
  | Tuple (e::t) -> eval_tuple env (Tuple t) (eval_head e,kE)
  | _            -> assert false (* n'arrive pas *)

and eval_match env e l (k,kE) =
  let rec try_match l v =
    match l with
    | []         -> raise PatternMismatchException
    | (m,e')::l' -> begin
      match find_match m v env with
      | Some env' -> eval_env env' e' (k,kE)
      | None      -> try_match l' v
      end
  in
  eval_env env e (try_match l,kE)

and eval_cons env e1 e2 (k,kE) =
  let k1 tl hd =
    match tl with
    | VList [] -> k (VList [hd])
    | VList l  -> k (VList (hd::l))
    | v        -> raise (ValueMismatchException.expList v)
  in
  let k2 tl = eval_env env e1 (k1 tl,kE) in
  eval_env env e2 (k2,kE)

and eval_trywith env e1 i e2 (k,kE) =
  let k_catch n = eval_env (Env.add env i n) e2 (k,kE) in
  eval_env env e1 (k,k_catch)

and eval_raise env e (k,kE) =
  let raise_now v =
    match v with
    | Int _ -> kE v
    | v     -> raise (ValueMismatchException.expInt v)
  in
  eval_env env e (raise_now,kE)
(**********************************************************************)
(* finalement, les fonctions d'évaluation d'une expression quelconque *)
(**********************************************************************)

(* évalue une expression dans un environnement *)
and eval_env env e (k,kE) =
  let k_couple = k,kE in
  match e with
  | Const c             -> k c
  | Var id              -> k (Env.get env id)
  | Oper_bin(op,e1,e2)  -> eval_operation_bin op env e1 e2 k_couple
  | Oper_unr(op,e)      -> eval_operation_unr op env e k_couple
  | Let(m,e1,e2)        -> eval_let env m e1 e2 k_couple
  | Ifte(e1,e2,e3)      -> eval_ifte env e1 e2 e3 k_couple
  | Print e             -> eval_print env e k_couple
  | Fun(i,e,r)          -> k (Closure (env,(i,e,r)))
  | App(e1,e2)          -> eval_app env e1 e2 k_couple
  | Seq(e1,e2)          -> eval_env env e1 ((fun _ -> eval_env env e2 k_couple),kE)
  | Ref e               -> eval_ref env e k_couple
  | Deref e             -> eval_deref env e k_couple
  | Assign(e1,e2)       -> eval_assign env e1 e2 k_couple
  | Tuple t             -> eval_tuple env e k_couple
  | Match(e,l)          -> eval_match env e l k_couple
  | EmptyList           -> k (VList [])
  | Cons(e1,e2)         -> eval_cons env e1 e2 k_couple
  | TryWith(e1,i,e2)    -> eval_trywith env e1 i e2 k_couple
  | Raise e             -> eval_raise env e k_couple
;;

(* définit l'environnement initial, avec les fonctions builtin *)
let intern_var = Var Intern_func_id
let define_builtin (s,e) =
  Id s, Closure(Env.empty_env, (MId Intern_func_id, e, None))

let builtins = [
  "prInt", Print(intern_var);
  "ref",   Ref(intern_var);
  "not",   Oper_unr(Oubb(Lnot), intern_var)
]

let builtin_env = List.map define_builtin builtins

let print_value v = print_string (string_of_value v); print_newline(); v
let identity x = x
let uncaughtexn v = raise (UncaughtExceptionException v)

(* évalue une expression avec les builtins, c'est le point de départ *)
let eval expr =
  eval_env builtin_env expr (identity,uncaughtexn)

