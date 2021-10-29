EXEC=fouine

TESTLEVEL=A # Avancé par défaut
ifneq ($(origin TESTVERBOSE), undefined) # Si la variable est définie, et quelle que soit sa valeur
	VERBOSITY="-v" # on active l'option '-v' dans les tests
endif


$(EXEC): main
	@mv main.native $(EXEC)

main:
	ocamlbuild -yaccflag -v -lib unix main.native

byte:
	ocamlbuild -yaccflag -v main.byte

clean:
	ocamlbuild -clean

test: $(EXEC)
	cd tests; ./autotest.sh $(VERBOSITY) $(TESTLEVEL) ../$(EXEC)

test_cps: $(EXEC)
	./autotest_cps.sh

test_types: $(EXEC)
	cd tests; ./types-test.sh $(VERBOSITY) T ../$(EXEC)

remake: | clean $(EXEC)