let f = function
  | x, y -> (prInt x; prInt y)
  | x -> prInt x
  | _ -> prInt 42
in
f (48, 5);
f 40
