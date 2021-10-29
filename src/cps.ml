open Affiche


(**************************)
(* Utilitaires de fichier *)
(**************************)
let prelude_cps = "./tests/preludeCPS.ml"
let prelude_caml = "./tests/preludeCaml.ml"
let in_file = "/tmp/in.ml"
let out_file = "/tmp/out"

let write_to_file file s =
  let oc = open_out file in
  let _ = Printf.fprintf oc "%s\n" s in
  close_out oc

let write_in = write_to_file in_file

let read_file file =
  let ic = open_in file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let read_out () = read_file out_file

let append_to_file file s =
  let _ = Sys.command("echo \"" ^ s ^ "\" >> " ^ file) in ()

let append_to_in = append_to_file in_file

let copy_file f1 f2 =
  let _ = Sys.command ("cp " ^ f1 ^ " " ^ f2) in ()


(***************************)
(* utilitaires d'exécution *)
(***************************)

let run_file_with program file outfile =
  let i = Sys.command (program ^ " " ^ file ^ " > " ^ outfile) in
  (* en vrai c'est un try et une exception et tout mais flemme *)
  if i<>0 then failwith ("Erreur d'execution (" ^ string_of_int i ^ ")")

let run_fouine () =
  run_file_with "./fouine" in_file out_file

let run_caml () =
  run_file_with "ocaml -w -A" in_file out_file


(***************************************)
(* les fonctions qui font tout marcher *)
(***************************************)

let outcode = ref false
let run = ref false
let autotest = ref false

let string_of_transformed e = "let main_transform = " ^ string_of_expr e ^ ";;"

(* -outcode *)
let affiche_transformed e =
  let s = string_of_transformed e in
  print_string s; print_newline ()


(* -run *)
let run_main = "main_transform ((fun x -> x),(fun x -> x))"

let run_transformed e =
  copy_file prelude_cps in_file;
  append_to_in (string_of_transformed e);
  append_to_in run_main;
  run_fouine ();
  print_string (read_out ())

(* -autotest *)
let run_autotest e t =
  let e_s = string_of_expr e in
  let t_s = string_of_transformed t in
  let e_fouine = write_in e_s; run_fouine (); read_out () in

  let ok s = print_string (if e_fouine = s then "OK\n" else "NO\n") in

  let e_caml = copy_file prelude_caml in_file; append_to_in e_s; run_caml (); read_out () in
  ok e_caml;

  let t_caml = copy_file prelude_caml in_file; append_to_in (read_file prelude_cps); append_to_in t_s; append_to_in run_main; run_caml (); read_out () in
  ok t_caml;

  let t_fouine = copy_file prelude_cps in_file; append_to_in t_s; append_to_in run_main; run_fouine (); read_out () in
  ok t_fouine


(*********************************************************)
(* Finalement, la fonction qui est appelée par `main.ml` *)
(*********************************************************)

let main e =
  let transformed = Transform.transform e in
  if !outcode then affiche_transformed transformed;
  if !run then run_transformed transformed;
  if !autotest then run_autotest e transformed

