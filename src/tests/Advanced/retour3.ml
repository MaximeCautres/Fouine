let r = ref 2 in
prInt (!r + (r:= 2* !r; !r))

