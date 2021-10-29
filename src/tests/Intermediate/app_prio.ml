let f = fun x -> x ;;
let _ = prInt(let x = 2 in f x) ;;
prInt(if f true then f 0 else f 1)
