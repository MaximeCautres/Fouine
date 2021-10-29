open Affiche

let showsrc = ref false
let debug = ref false
let trace = ref false

let print_int_debug ( n : int ) =
  if not !showsrc || !debug
  then print_int n; print_newline ()

let affiche_expr_debug e =
  if !showsrc || !debug
  then begin
    print_string (string_of_expr e);
    print_newline ()
  end

