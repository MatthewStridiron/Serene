#!/bin/bash

# opam install llvm
# brew install llvm

eval $(opam env)
rm -rf _build

ocamlbuild -pkgs llvm serene.native
./serene.native -l tests/example.sn > example.out
/usr/local/opt/llvm/bin/llc example.out
gcc -S helper.c
gcc example.out.s helper.s -o example.run
./example.run
