open Eval
open Typage

(* l'endroit où on lit le code fouine, stdin par défaut *)
let input_channel = ref stdin


(**************************************************************************)
(* les incantations magiques qui envoient le texte au parser, et évaluent *)
(**************************************************************************)

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)
let parse () =
  (* ligne de debug magique qui affiche le parcours de l'automate ! *)
  (* ouvre lexbuf *)
  let lexbuf = Lexing.from_channel !input_channel in
  let _ = Stdlib.Parsing.set_trace !Debug.trace in
  Parser.main Lexer.token lexbuf

let compile e =
  Debug.affiche_expr_debug e;
  Typage.main e;
  ignore (eval e)

let calc e =
	  compile e; flush stdout


(***************************************************)
(* enregistrement des options de ligne de commande *)
(***************************************************)

let main = ref calc

(* change la fonction lancée, en fonction de l'option CPS *)
let set_cps () = main := Cps.main

let create_channel s =
  let ic =
  match s with
  | "" -> stdin
  | _  -> open_in s
  in input_channel := ic

let options = [
  ("-showsrc", Arg.Set Debug.showsrc, "Affiche le code bien parenthésé sans les résultats");
  ("-debug", Arg.Set Debug.debug, "Affiche le code bien parenthésé avec les résultats" );
  ("-trace", Arg.Set Debug.trace, "Affiche la trace de l'automate\n");

  ("-cps", Arg.Unit set_cps, "Applique la transformation en CPS");
  ("-outcode", Arg.Set Cps.outcode, "\tAffiche le programme résultant de la transformation");
  ("-run", Arg.Set Cps.run, "\t\tExécute en Fouine le programme transformé");
  ("-autotest", Arg.Set Cps.autotest, "\tCompare les exécutions du programme transformé\n");

  ("-notypes", Arg.Set Typage.notypes, "désactive l'inférence de types et interprète directement le programme");
  ("-showtypes", Arg.Set Typage.showtypes, "affiche le type inféré pour toutes les déclarations en surface\n");
  ("-showpb", Arg.Set Typage.showpb, "affiche le problème d'unification");
  ("-no-sharing", Arg.Set Typage.no_sharing, "n'utilise PAS l'algorithme d'unification avec sharing (performances déplorables)");
  ("-mono", Arg.Set Inference.mono, "type avec monomorphisme plutôt que polymorphime")
]

let usage = "usage : ./fouine [-showsrc] [-debug] [-trace] [-cps [-outcode] [-run] [-autotest]] [file]"

let _ = Arg.parse options create_channel usage

let _ =
  let result = parse () in
  !main result

