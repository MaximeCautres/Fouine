open Expr
open TypeUtils

let mono = ref false (* devra être à false par défaut *)

(**********************************)
(* utilitaire de pattern matching *)
(**********************************)

let type_of_type t1 = match t1 with
  | Int _   -> TOp(TInt,[]) 
  | Bool _  -> TOp(TBool,[])
  | Unit    -> TOp(TUnit,[])
  | _ -> failwith "pas implémenté"

let rec find_match m v env is_let = match m with
  | MConst t -> begin 
    p_append(TVar v, type_of_type t); 
    env
    end
  | MId id -> begin
    let env' = add env id None in
    if id <> Env.Void
    then
      let v1 = fst (List.hd env') in
      (if is_let then ajoute_surface v1);
      p_append(TVar v, TVar v1);
    else ();
    env'
    end
  | MTuple mlist -> begin
    let rec inter l env = match l with
      | [] -> [], env
      | m' :: l' -> begin
        let v1 = get_next_var () in 
        let env' = find_match m' v1 env is_let in
        let vlist, env'' = inter l' env' in 
        (TVar(v1) :: vlist, env'')
      end
      in let vlist, env' = inter mlist env in 
      p_append(TVar v, TOp(TTuple,vlist));
      env'
    end
  | MNil -> env 
  | MCons(m1, m2) -> begin
    let v1 = get_next_var () in 
    let v2 = get_next_var () in 
    p_append (TVar v, TOp(TList, [TVar v1]));
    p_append (TVar v, TVar v2);
    let env' = find_match m1 v1 env is_let in
    find_match m2 v2 env' is_let
    end


(*******************************************)
(* Les fonctions s'occupant de l'inférence *)
(*******************************************)

and inference_oper_bin v env o e1 e2 = match o with
  | Obpb _ -> begin
    let v1 = get_next_var () in
    let v2 = get_next_var () in
    p_append (TVar v, TOp(TBool, []));
    p_append (TVar v1, TVar v2);
    inference_main e1 v1 env;
    inference_main e2 v2 env
    end
  | _ -> let t2, t1 = type_of_op o in
    begin
      let v1 = get_next_var () in 
      let v2 = get_next_var () in
      p_append (TVar v, t1);
      p_append (TVar v1, t2);
      p_append (TVar v2, t2);
      inference_main e1 v1 env;
      inference_main e2 v2 env
    end;

and type_of_op = function
  | Obaa _ -> (TOp(TInt, []), TOp(TInt, []))
  | Obbb _ -> (TOp(TBool, []), TOp(TBool, []))
  | Obab _ -> (TOp(TInt, []), TOp(TBool, []))
  | _ -> failwith "Le dernier cas est traité séparément"

and inference_oper_unr v env o e = match o with
  | Oubb o' -> begin
    let v = get_next_var () in 
    p_append(TVar v, TOp(TBool, []));
    p_append (TVar v, TOp(TBool, []));
    inference_main e v env;
  end
    
(*and inference_let_mono v env m e1 e2 = 
  begin
    let v1 = get_next_var () in (* variable de m *)
    let v2 = get_next_var () in (* variable de e1 *)
    let v3 = get_next_var () in (* variable de e2 *)
    p_append (TVar v, TVar v3); 
    p_append (TVar v1, TVar v2);
    let env' = find_match m v1 env true in
    incr profondeur;
    inference_main e1 v2 env;   (* on crée le problème d'unification de e1 *)
    decr profondeur;
    inference_main e2 v3 env'

  end *)

and inference_let v env m e1 e2 = 
begin
  let vm = get_next_var () in (* variable de m *)
  let v1 = get_next_var () in (* variable de e1 *)
  let v2 = get_next_var () in (* variable de e2 *)

  incr profondeur;
  inference_main e1 v1 env;   (* on crée le problème d'unification de e1 *)
  
  let substitution = UnifSharing.unify !p in     (* ici on génère le type de v *)
  
  let t1 = match List.assoc_opt v1 substitution with (* On recupère le type de e1 *)
  | Some t_v' -> t_v'
  | None -> failwith "ne doit pas arriver"
  in 

  let analyse_type_for_poly id typ env env_bfl = (* env_bfl pour env_before_let, savoir si v' est dans l'env d'avant le let ou non *)
    let rec inter env_bfl fa ty = match ty with
      | TVar(v') -> begin
        match List.assoc_opt v' env_bfl with
        | Some _ -> fa
        | None -> v' :: fa
        end
      | TOp(_, tl) -> List.fold_left (inter env_bfl) fa tl
    in 
    if !mono then add env id None else add env id (Some (inter env_bfl [] typ, typ))
  in
  let rec unbuilt_for_poly m typ env env_bfl is_let = match m, typ with (* on trouve les parti du type correspond a des nouvelle variable à ajouter *)
    | MConst (Int _), TOp(TInt, _) -> env 
    | MConst (Bool _), TOp(TBool, _) -> env
    | MConst (Unit), TOp(TUnit, _) -> env
    | MId id, t' -> let env' = analyse_type_for_poly id t' env env_bfl in
                  if id <> Env.Void
                  then
                    let v1 = fst (List.hd env') in
                    (if is_let then ajoute_surface v1);
                  else ();
                  env'
    | MTuple(ml), TOp(TTuple, tl) -> (
      try List.fold_left (fun x (y, z) -> unbuilt_for_poly y z x env_bfl is_let) env (List.combine ml tl)
      with Invalid_argument _-> failwith "pas possible d'unifier car tuple pas de la même taille")
    | MCons(m1,m2), TOp(TList, [t']) -> unbuilt_for_poly m2 (TOp(TList, [t'])) (unbuilt_for_poly m1 t' env env_bfl is_let) env_bfl is_let
    | MNil, TOp(TList, [t']) -> env
    | _, _ -> failwith "Si on est la c'est que les motif corresponde pas"
  in
  let env' = unbuilt_for_poly m t1 env env true in 
  decr profondeur;
  p_append (TVar v, TVar v2); 
  p_append (TVar vm, TVar v1);
  inference_main e2 v2 env'

end
    



and inference_var v env id = 
  let (v1, (fal, t1)) = get env id in 
  let subst_list = List.map (fun x -> (x, match fst x with Some id' -> get_next_named_var id' | None -> get_next_var ())) fal in 
  let do_subst v' sl = match List.assoc_opt v' sl with
    | Some v'' -> v''
    | None     -> v'
    in
  let rec applysubst sl typ = match typ with
  | TVar v' -> TVar (do_subst v' sl)
  | TOp(op,tlist) -> TOp(op,List.map (applysubst sl) tlist) in
  p_append(TVar v, applysubst subst_list t1)

and inference_ifte v env b e1 e2 =
  begin
    let vb = get_next_var () in
    let v1 = get_next_var () in
    let v2 = get_next_var () in
    p_append (TVar vb, TOp(TBool, []));
    p_append (TVar v1, TVar v);
    p_append (TVar v2, TVar v);
    inference_main b vb env;
    inference_main e1 v1 env;
    inference_main e2 v2 env
  end

and inference_fun v env i e r = 
  begin
    let v1 = get_next_var () in
    let v2 = get_next_var () in 
    p_append (TVar v, TOp(TFleche,[TVar v1; TVar v2]));
    let env' = find_match i v1 env false in
    match r with 
    | None -> inference_main e v2 env'
    | Some id -> begin 
      let v3 = get_next_var () in
      let env'' = find_match (MId id) v3 env' false in
      p_append (TVar v3, TVar v);
      inference_main e v2 env''
    end
  end

and inference_app v env e1 e2 = 
  begin
    let v1 = get_next_var () in
    let v2 = get_next_var () in
    p_append (TVar v1, TOp(TFleche, [TVar v2; TVar v]));
    inference_main e1 v1 env;
    inference_main e2 v2 env
  end

and inference_tuple v env t = 
  let rec inter l env = match l with 
    | [] -> []
    | e::l' -> begin
      let v1 = get_next_var () in
      inference_main e v1 env;
      (TVar v1) :: (inter l' env)
    end
  in p_append (TVar v, TOp(TTuple, inter t env))

and inference_empty_list v env = 
  let v1 = get_next_var () in
  p_append (TVar v, TOp(TList, [TVar v1]))

and inference_cons v env e1 e2 = 
  begin 
    let v1 = get_next_var () in
    let v2 = get_next_var () in
    p_append(TVar v, TOp(TList, [TVar v1]));
    p_append(TVar v, TVar v2);
    inference_main e1 v1 env;
    inference_main e2 v2 env;
  end

and inference_ref v env e = 
  begin
    let v1 = get_next_var () in
    p_append (TVar v, TOp(TRef, [TVar v1]));
    inference_main e v1 env
  end

and inference_deref v env e = 
  begin
    let v1 = get_next_var () in
    p_append(TVar v1, TOp(TRef, [TVar v]));
    inference_main e v1 env
  end

and inference_assign v env e1 e2 = 
  begin 
    let v1 = get_next_var () in
    let v2 = get_next_var () in
    p_append(TVar v, TOp(TUnit,[]));
    p_append(TVar v1, TOp(TRef, [TVar v2]));
    inference_main e1 v1 env;
    inference_main e2 v2 env
  end

and inference_seq v env e1 e2 = 
  begin
    let v1 = get_next_var () in
    let v2 = get_next_var () in
    p_append(TVar v, TVar v2);
    inference_main e1 v1 env;
    inference_main e2 v2 env
  end

and inference_match v env e l = 
  let ve = get_next_var () in
  begin
    inference_main e ve env;
    let inter (mi, ei) = 
    begin
      let vmi = get_next_var () in
      let vei = get_next_var () in 
      p_append(TVar v, TVar vei);
      p_append(TVar ve, TVar vmi);
      let env' = find_match mi vmi env false in 
      inference_main ei vei env'
    end
    in List.iter inter l
  end

and inference_trywith v env e1 i e2 = 
  let ve1 = get_next_var () in
  let ve2 = get_next_var () in
  let vi = get_next_var () in 
  p_append(TVar v, TVar ve1);
  p_append(TVar v, TVar ve2);
  let env' = find_match (MId i) vi env false in
  inference_main e1 ve1 env;
  inference_main e2 ve2 env'

and inference_raise v env e = 
  let ve = get_next_var () in
  p_append(TVar ve, TOp(TInt, []));
  inference_main e ve env

and inference_main (e : Expr.expr) v env = match e with
  | Const(Int _)            -> p_append (TVar v, TOp(TInt, []))
  | Const(Bool _ )          -> p_append (TVar v, TOp(TBool, []))
  | Const Unit              -> p_append (TVar v, TOp(TUnit, []))
  | Var(id)                 -> inference_var v env id 
  | Oper_bin(o,e1,e2)       -> inference_oper_bin v env o e1 e2 
  | Oper_unr(o,e)           -> inference_oper_unr v env o e
  | Let(m,e1,e2)            -> inference_let v env m e1 e2
  | Ifte(b,e1,e2)           -> inference_ifte v env b e1 e2
  | Fun(i,e,r)              -> inference_fun v env i e r
  | App(e1,e2)              -> inference_app v env e1 e2
  | Seq(e1,e2)              -> inference_seq v env e1 e2
  | Ref e                   -> inference_ref v env e
  | Deref e                 -> inference_deref v env e
  | Assign(e1,e2)           -> inference_assign v env e1 e2
  | Tuple t                 -> inference_tuple v env t
  | Match(e,l)              -> inference_match v env e l 
  | EmptyList               -> inference_empty_list v env
  | Cons(e1,e2)             -> inference_cons v env e1 e2
  | TryWith(e1,i,e2)        -> inference_trywith v env e1 i e2
  | Raise e                 -> inference_raise v env e
  | _ -> assert false

let depart =
  let v0 = get_next_var () in
  [
    ("prInt", TOp(TFleche, [TOp(TInt, []); TOp(TInt, [])]), []);
    ("not", TOp(TFleche, [TOp(TBool, []); TOp(TBool, [])]), []);
    ("ref", TOp(TFleche, [TVar v0; TOp(TRef, [TVar v0])]), [v0])
  ]

let inference e = 
  let depart_var = List.map (fun (s,t,u) -> get_next_named_var (Env.Id s), t, u) depart in
  List.iter (fun (v,t,_) -> p_append (TVar v, t)) depart_var;
  let v = get_next_var () in inference_main e v (List.map (fun (x,y,z) -> (x , (z, y))) depart_var); !p, v

