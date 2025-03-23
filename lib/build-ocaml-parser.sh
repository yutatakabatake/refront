#!/bin/zsh
# このファイルがocamlyacc_testにあることを仮定している

ocamlc -c syntax.ml
ocamlc -c ast_to_sexpr.ml
ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c main.ml
ocamlc -o main lexer.cmo parser.cmo ast_to_sexpr.cmo main.cmo syntax.cmo 

# ./main < program/program-Bob.txt