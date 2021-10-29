let f x = match x with
    y, 3 -> y * y
  | _    -> -1
  | _,_ -> prInt 0

in prInt(f (2, 3)); prInt(f (3,4))

