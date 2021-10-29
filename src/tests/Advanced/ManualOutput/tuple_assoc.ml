match 1,(2,3) with
| x,y,z -> prInt 42
| x,(y,z) -> prInt (x+y+z)
