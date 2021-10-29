let z = 20 ;;
let f t = if t >= 12 then raise (E 17) else [5;3];;
let n = try f z with E x -> [x+1] ;;
match n with
| n::l -> prInt n
| _ -> prInt 0
