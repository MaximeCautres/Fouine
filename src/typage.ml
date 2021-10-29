open Affiche
open Exceptions
open TypeUtils

let notypes = ref false
let showtypes = ref false
let no_sharing = ref false
let showpb = ref false

let main e =
  if not !notypes then
    let pb, tipe = Inference.inference e in
    let _ = if !showpb then affiche_pb pb in
    let unify = if !no_sharing then Unif.unify else UnifSharing.unify in
    let sub = unify pb in
    if !showtypes then (print_newline(); affiche_surface sub (List.rev !id_surface) tipe)

