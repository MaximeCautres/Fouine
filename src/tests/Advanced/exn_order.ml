try
  (prInt 1, raise (E 2), prInt 3, raise (E 4), prInt 5)
with E n -> (prInt n; 0,0,0,0,0)
