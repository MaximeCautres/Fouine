let rec ack = fun m -> fun n ->
  if m = 0 then n+1
  else if m>0 && n=0 then ack (m-1) 1
  else ack (m-1) (ack m (n-1))
;;
prInt(ack 3 4)

