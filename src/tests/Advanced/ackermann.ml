let rec ackermann x y=
  match x,y with
  | 0,n -> n+1
  | m,n -> ackermann (m-1) (
    match n with
    | 0 -> 1
    | _ -> ackermann m (n-1)
  )
in
prInt (ackermann 3 2)
