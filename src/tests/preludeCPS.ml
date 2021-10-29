let prInt x (k,kE) = prInt x; k x;;
let ref x (k,kE) = k (ref x);;
let not x (k,kE) = k (not x);;
