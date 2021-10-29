let x = ref 2;;
[prInt (!x + 10);
 prInt (x := !x + 1; !x);
 if !x=2 then (x:=!x+1; prInt !x) else (x:=0; prInt !x)
]
