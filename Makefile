build:
	dune build

# code:
# 	-dune build
# 	code .
# 	! dune build --watch

# utop:
# 	OCAMLRUNPARAM=b dune utop src

# test:
# 	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec game/main.exe

# zip:
# 	rm -f ms3_code.zip
# 	zip -r ms3_code.zip . -x@exclude.lst

# clean:
# 	dune clean
# 	rm -f ms3_code.zip

# doc:
# 	dune build @doc

# opendoc: doc
# 	@bash opendoc.sh