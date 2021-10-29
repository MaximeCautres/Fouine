let g = fun x -> x+1 ;;
let f = fun x -> fun y -> fun z -> x + y + z;;
prInt(f 3 5 (g 7))

