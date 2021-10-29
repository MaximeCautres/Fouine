%{

open Expr
open Env

(*
 * renvoie une fonction récursive si b est vrai,
 * ne change rien si b est faux ou e n'est pas une fonction
 *)
let set_rec b name e =
  match e with
  | Fun(m,e',_) -> Fun(m, e', if b then Some name else None)
  | _           -> e

(*
 * les fonctions qui suivent sont définies ici parce que
 * ça fait des longues lignes lourdes au milieu du parser
 *)

let function_to_match e =
  Fun(MId(Intern_func_id), Match(Var(Intern_func_id), e), None)

(* let motif = e1 in e2 *)
let make_let (m,e1) e2 = Let(m,e1,e2)

%}

/**************************/
/* définitions des tokens */
/**************************/

%token <int> INT
%token PLUS TIMES MINUS DIVIDE
%token LPAREN RPAREN
%token EOF

%token LET EQUAL IN UND DSEMCOL
%token IF THEN ELSE
%token LT GT LEQ GEQ NEQ LAND LOR
%token SEMCOL BANG COLEQ
%token <string> ID
%token <bool> BOOL
%token FUN ARROW REC
%token COMMA
%token MATCH WITH PIPE
%token LBRACKET RBRACKET COLCOL
%token FUNCTION
%token BEGIN END
%token TRY WITH E RAISE


/*************/
/* priorités */
/*************/

/* prios basses pour que ça marche, mais on est pas sûr */
%nonassoc ARROW /* après on a toujours fun x -> (...), jamais (fun x -> ...) (...) */
%nonassoc PIPE /* plus forte que flèche pour les match... */

/*
 * prios de OCaml
 * cf https://caml.inria.fr/pub/docs/manual-ocaml/expr.html#ss:precedence-and-associativity
 * celles après le commentaire vide sont rajoutées par nous
 */
%nonassoc LET MATCH FUN FUNCTION TRY WITH
%nonassoc LOW_SEMCOL
%right SEMCOL
%nonassoc IF /**/ ELSE
%right COLEQ
%nonassoc LOW_COMMA
%left COMMA /* c'est nonassoc en Caml (et c'est logique) mais ça marche mieux comme ça */
%left OP
%right LOR
%right LAND
%left EQUAL LT GT NEQ /**/ LEQ GEQ
%right COLCOL
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS


/**************************************/
/* non-terminal de départ et son type */
/**************************************/

%type <Expr.expr> main
%start main

%%

/***********************************************************************/
/* le point de départ des expressions, avec des let in au début ou pas */
/***********************************************************************/

main:
  expression EOF    { $1 }
;

expression:
  | letin_surface   { $1 }
  | nolet_seq       { $1 }
;


/**********************************/
/* les toutes petites expressions */
/**********************************/

id:
  | ID              { Id $1 }
  | UND             { Void }
;

var:
  | ID              { Id $1 }
;

constant:
  | BOOL            { Bool $1 }
  | INT             { Int $1 }
  | LPAREN RPAREN   { Unit }
;


/******************/
/* les opérations */
/******************/

oper:
  | obaa { let e1,op,e2 = $1 in Oper_bin(Obaa op, e1, e2) }
  | obbb { let e1,op,e2 = $1 in Oper_bin(Obbb op, e1, e2) }
  | obab { let e1,op,e2 = $1 in Oper_bin(Obab op, e1, e2) }
  | obpb { let e1,op,e2 = $1 in Oper_bin(Obpb op, e1, e2) }
  | ouaa { $1 }
;

obaa: /* operateur_binaire_arith_arith: */
  | expr_profonde PLUS   expr_profonde  { $1, Add, $3 }
  | expr_profonde TIMES  expr_profonde  { $1, Mul, $3 }
  | expr_profonde MINUS  expr_profonde  { $1, Min, $3 }
  | expr_profonde DIVIDE expr_profonde  { $1, Div, $3 }
;

obbb: /* operateur_binaire_bool_bool: */
  | expr_profonde LAND expr_profonde   { $1, Land, $3 }
  | expr_profonde LOR  expr_profonde   { $1, Lor, $3}
;

obab: /* operateur_binaire_arith_bool: */
  | expr_profonde LT  expr_profonde    { $1, Lt, $3}
  | expr_profonde GT  expr_profonde    { $1, Gt, $3}
  | expr_profonde LEQ expr_profonde    { $1, Leq, $3 }
  | expr_profonde GEQ expr_profonde    { $1, Geq, $3 }
;

obpb: /* operateur_binaire_polymorphe_bool: */
  | expr_profonde EQUAL expr_profonde  { $1, Equal, $3 }
  | expr_profonde NEQ   expr_profonde  { $1, Neq, $3 }
;

ouaa:
  | MINUS expr_atomique %prec UMINUS { Oper_bin(Obaa Min, Const (Int 0), $2) }

/* operateur_unaire_bool_bool: le not est une fonction ! */

/**************/
/* les motifs */
/**************/

motif_atomique:
  | id                                    { MId $1 }
  | constant                              { MConst $1 }
  | LPAREN motif RPAREN                   { $2 }
  | LBRACKET motif_liste RBRACKET         { $2 }
  | LBRACKET RBRACKET                     { MNil }
;

motif_liste:
  | motif SEMCOL motif_liste              { MCons($1,$3) }
  | motif                                 { MCons($1,MNil) }
;

motif_cons:
  | motif_atomique COLCOL motif_cons      { MCons($1,$3) }
  | motif_atomique COLCOL motif_atomique  { MCons($1,$3) }
;

/*
 * (x::y,z) est parsé comme (x::y),z
 * donc entre les virgules il n'y a pas de tuples sans parenthèses
 */
motif_notup:
  | motif_atomique                        { $1 }
  | motif_cons                            { $1 }
;

motif_tuple:
  | motif_notup COMMA motif_tuple         { $1::$3 }
  | motif_notup COMMA motif_notup         { [$1;$3] }
;

motif:
  | motif_tuple                           { MTuple $1 }
  | motif_notup                           { $1 }
;


/*************/
/* les match */
/*************/

match_case:
  | motif ARROW seqlist PIPE match_case { ($1,$3)::$5 }
  | motif ARROW seqlist                 { [($1,$3)] }
;

debut_match_case:
  | PIPE match_case                     { $2 }
  | match_case                          { $1 }
;


/********************/
/* les let (rec) in */
/********************/

debut_decl:
  | LET motif EQUAL seqlist           { $2,$4 }
  | LET id let_arglist                { MId $2, set_rec false $2 $3 }
  | LET REC id let_arglist            { MId $3, set_rec true $3 $4 }
  | LET REC id EQUAL seqlist          { MId $3, set_rec true $3 $5 }

let_arglist:
  | motif let_arglist                 { Fun($1,$2,None) }
  | motif EQUAL seqlist               { Fun($1,$3,None) }
;

fun_arglist:
  | motif fun_arglist                 { Fun($1,$2,None) }
  | motif ARROW seqlist               { Fun($1,$3,None) }
;

/*
 * la dernière expression de la suite de
 * let...=...in let...=...in...
 * est une séquence qui ne commence pas par une let
 */
letin_surface:
  | debut_decl IN seqlist             { make_let $1 $3 }

  | debut_decl DSEMCOL letin_surface  { make_let $1 $3 }
  | debut_decl DSEMCOL nolet_seq      { make_let $1 $3 }
;


/*****************/
/* les séquences */
/*****************/

/* une séquence d'expressions profonde, qui peut finir par un ; */
seqlist:
  | expr_profonde  %prec LOW_SEMCOL { $1 }
  | expr_profonde SEMCOL            { $1 }
  | expr_profonde SEMCOL seqlist    { Seq($1,$3) }
;

/* une séquence qui ne commence pas par un let */
nolet_seq:
  | expr_nolet                      { $1 }
  | expr_nolet SEMCOL seqlist       { Seq($1,$3) }
;


/**********************************************/
/* les types composés "liste": liste et tuple */
/**********************************************/

tuple_expr:
  | tuple_expr COMMA expr_profonde    { $3::$1 }
  | expr_profonde COMMA expr_profonde { [$3; $1] }
;

list_ind:
  | expr_profonde SEMCOL list_ind     { Cons($1,$3) }
  | expr_profonde                     { Cons($1,EmptyList) }
;


/**************************************/
/* les expressions les plus générales */
/**************************************/

/* une suite d'applications de fonction */
expr_apply:
  | expr_apply expr_atomique                          { App($1,$2) }
  | expr_atomique expr_atomique                       { App($1,$2) }
;

/* une exception avec un pipe ou pas... */
catch_exception:
  | PIPE E id ARROW seqlist                           { $3,$5 }
  |      E id ARROW seqlist                           { $2,$4 }
;

/* une expression qui n'est pas la combinaison de plusieurs expressions */
expr_atomique:
  | var                                               { Var $1 }
  | constant                                          { Const $1 }
  | BANG expr_atomique                                { Deref $2 }
  | LBRACKET RBRACKET                                 { EmptyList }
  | LBRACKET list_ind RBRACKET                        { $2 }
  | LPAREN seqlist RPAREN                             { $2 }
  | BEGIN seqlist END                                 { $2 }
;

/* une expression qui ne commence pas par un let */
expr_nolet:
  | expr_atomique                                     { $1 }
  | oper                                              { $1 }
  | IF seqlist THEN expr_profonde ELSE expr_profonde  { Ifte($2,$4,$6) }
  | FUN fun_arglist                                   { $2 }
  | expr_profonde COLEQ expr_profonde                 { Assign($1,$3) }
  | expr_profonde COLCOL expr_profonde                { Cons($1,$3) }
  | expr_apply                                        { $1 }
  | tuple_expr %prec LOW_COMMA                        { Tuple (List.rev $1) }
  | MATCH seqlist WITH debut_match_case               { Match($2,$4) }
  | FUNCTION debut_match_case                         { function_to_match $2 }
  | TRY seqlist WITH catch_exception                  { let n,e = $4 in TryWith($2,n,e) }
  | RAISE LPAREN E expr_atomique RPAREN               { Raise $4 }
;

/*
 * enfin, presque n'importe quelle expression
 * on ne peut pas mettre de ;; dedans
 */
expr_profonde:
  | expr_nolet                       %prec LOW_SEMCOL { $1 }
  | debut_decl IN seqlist                             { make_let $1 $3 }
;

