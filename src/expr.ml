open Env


(**************************)
(* "petits" types de base *)
(**************************)

type value =
  | Int of int
  | Bool of bool
  | Closure of (value environment) * funct
  | Address of int
  | Unit
  | VTuple of value list
  | VList of value list

(* type des valeurs fonctions *)
(* Si l'option est (Some f), alors la fonction est récursive et s'appelle f *)
and funct = motif * expr * (identifier option)

and motif =
  | MConst of value
  | MId of identifier
  | MTuple of motif list
  | MCons of motif*motif
  | MNil


(************************)
(* types des operateurs *)
(************************)

and arith_arith_bin_op = Add | Mul | Min | Div

and arith_bool_bin_op = Gt | Leq | Geq | Lt

and plmrf_bool_bin_op = Neq | Equal

and bool_bool_bin_op = Land | Lor

and bool_bool_unr_op = Lnot

and operation_bin =
  | Obaa of arith_arith_bin_op

  | Obpb of plmrf_bool_bin_op

  | Obab of arith_bool_bin_op
  | Obbb of bool_bool_bin_op

and operation_unr =
  | Oubb of bool_bool_unr_op

(************************)
(* type des expressions *)
(************************)

and expr =
  | Const      of value
  | Var        of identifier
  | Oper_bin   of operation_bin*expr*expr
  | Oper_unr   of operation_unr*expr
  | Let        of motif*expr*expr
  | Ifte       of expr*expr*expr
  | Print      of expr
  | Fun        of funct
  | App        of expr*expr
  | Seq        of expr*expr
  | Ref        of expr
  | Deref      of expr
  | Assign     of expr*expr
  | Tuple      of expr list
  | Match      of expr*((motif*expr) list)
  | EmptyList
  | Cons       of expr*expr
  | TryWith    of expr*identifier*expr
  | Raise      of expr

