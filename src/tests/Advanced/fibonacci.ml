let fibonacci = function | k ->
  let rec aux = function
  | 0 -> 0, 1
  | n -> match  aux (n-1) with
          | m, p -> p, m + p
  in match aux k with
  | x, _ -> x
in prInt (fibonacci 4)
