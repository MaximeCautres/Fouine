let _ = prInt(if false && false then 1 else 0);;
let _ = prInt(if false && true then 1 else 0);;
let _ = prInt(if true && false then 1 else 0);;
let _ = prInt(if true && true then 1 else 0);;

let _ = prInt(if false || false then 1 else 0);;
let _ = prInt(if false || true then 1 else 0);;
let _ = prInt(if true || false then 1 else 0);;
let _ = prInt(if true || true then 1 else 0);;

let _ = prInt(if not true then 1 else 0);;
        prInt(if not false then 1 else 0)

