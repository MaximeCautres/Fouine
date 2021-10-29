let f = fun x y z -> 2*x - y/2 + z;;
let g = fun x y -> f x y 3;;
prInt(g 7 9)

