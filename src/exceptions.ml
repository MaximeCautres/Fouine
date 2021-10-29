open Expr
open Affiche
open Env
open Mem

module ValueMismatchException =
  struct
    let make s x = let module E =
      struct
        exception E of value
        let _ = Printexc.register_printer
        (function
          | E v -> Some (Printf.sprintf "\nValueMismatchException: on attendait %s mais il y a %s à la place\n\tValeur inattendue : %s" s (name_of_val v) (string_of_value v))
          | _   -> None
          )
      end
      in E.E x

    let make_of_val v = make (name_of_val v)

    let expInt      = make_of_val (Int 0)
    let expBool     = make_of_val (Bool false)
    let expClosure  = make_of_val (Closure (Env.empty_env, (MNil,Const Unit, None)))
    let expAddress  = make_of_val (Address 0)
    let expUnit     = make_of_val  Unit
    let expTuple    = make_of_val (VTuple [])
    let expList     = make_of_val (VList [])
  end

exception PatternMismatchException
exception UncaughtExceptionException of value
exception NotUnifiableException of TypeUtils.t * TypeUtils.t

let _ = Printexc.register_printer (function
  | Stdlib.Parsing.Parse_error    -> Some ("erreur de syntaxe")
  | EnvironmentMissException id   -> Some (Printf.sprintf "\nEnvironmentMissException: La variable `%s` n'est pas dans l'environnement" (string_of_id id))
  | PatternMismatchException      -> Some ("\nPatternMismatchException: erreur de matching")
  | OutOfMemoryException          -> Some ("\nOutOfMemoryException: pas assez de mémoire")
  | UncaughtExceptionException v  -> Some (Printf.sprintf "\nUncaughtExceptionException: exception non rattrapée\n\tException levée : E %s" (string_of_value v))
  | NotUnifiableException (t1,t2) -> Some (Printf.sprintf "\nNotUnifiableException: le programme n'est pas typable\n\tTypes incompatibles : %s | %s" (string_of_type t1) (string_of_type t2))
  | _                             -> None
  )

