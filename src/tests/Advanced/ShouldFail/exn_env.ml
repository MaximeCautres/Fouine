try
	let x = 2 in
	raise (E (x+1))
with
| E a -> prInt a; x
