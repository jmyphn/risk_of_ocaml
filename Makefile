.PHONY: test check

build:
	dune clean
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec game/main.exe

zip:
	rm -f riskofocaml.zip
	zip -r riskofocaml.zip . -x@exclude.lst

clean:
	dune clean
	rm -f *.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh