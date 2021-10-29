exception OutOfMemoryException

let max_mem = 100

let mem = Array.make max_mem (Expr.Unit) (* taille fixe pour l'instant *)

let adr = ref 0 (* pointeur vers la prochaine case à allouer *)

let add v =
    if !adr >= max_mem then raise OutOfMemoryException else begin
    mem.(!adr) <- v;
    incr adr;
    !adr - 1
  end

(*
 * `a` est par définition une adresse renvoyée par `add`,
 * donc une adresse valide
 *)

let get a = mem.(a)

let set a v = begin
  mem.(a) <- v
end

