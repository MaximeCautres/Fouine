let _ = prInt(if false && false || true then 1 else 0);;
let _ = prInt(if false && false && true then 1 else 0);;
let _ = prInt(if true || true && false then 1 else 0);;
let _ = prInt(if not false && false then 1 else 0);;
        prInt(if not true || true then 1 else 0)

