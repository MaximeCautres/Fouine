let [x] = begin
  match [(1,2,[3;4]);(5,6,[])], 8 with
  | [],_ -> prInt (-1)
  | [(x,y,z::[t]); (u,v,[])], w -> prInt (x+y+z+t+u+v+w)
  | _ -> prInt (-2)
  end
  ::[]
in
prInt x

