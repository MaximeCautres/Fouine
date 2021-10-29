try
	raise (E (raise (E 2)))
with
| E n -> prInt n
