let rec fibo = fun n ->
  if n<=1 then n
  else
    (fibo (n-1)) + (fibo (n-2))
;;
prInt(fibo 7)
