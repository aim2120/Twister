cd src

ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex scanner.mll
ocamlc -c scanner.ml

cd ..
