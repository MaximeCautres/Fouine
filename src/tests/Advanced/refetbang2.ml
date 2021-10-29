let x = ref 5;;
let y = ref 7;;
prInt(!(if (!x < !y) then x else y))
