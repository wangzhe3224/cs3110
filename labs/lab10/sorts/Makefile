test:
	ocamlbuild -use-ocamlfind test_sorts.byte && ./test_sorts.byte
	
report:
	bisect-report -I _build -html report bisect*.out
	
clean:
	ocamlbuild -clean
	rm -f bisect*.out
	rm -rf report
	