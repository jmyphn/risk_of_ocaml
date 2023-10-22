# risk_of_ocaml

## Installation and Dependencies
This project depends on Raylib and Raygui. Use the package manager [opam](https://opam.ocaml.org/) to install these dependencies.

In a terminal instance, run
```bash
opam depext raylib
```
This command will install the C dependencies of Raylib. Now run
```bash
opam install raylib
```
to install the Raylib library for OCaml. Then run
```bash
opam install raygui
```
to install Raygui for OCaml.

## Running the game
In the source directory of the project, run
```bash
dune build
```
to build the project. Then run
```bash
make play
```
to run [risk_of_ocaml](https://github.coecis.cornell.edu/jp2369/3110proj) in terminal.