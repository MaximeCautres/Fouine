let compose f g = fun x -> f (g x);;
let rec power f n =
  if n = 0 then fun x -> x
  else compose f (power f (n-1))
;;
let f = power (fun x -> x*x);;
prInt(f 3 3)
