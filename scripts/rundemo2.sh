#!/bin/bash

# opam install llvm
# brew install llvm

eval $(opam env)
rm -rf _build

ocamlbuild -pkgs llvm serene.native
gcc -S helper.c

./serene.native -l demos/demo2.sn > demos/demo2.out
/usr/local/opt/llvm/bin/llc demos/demo2.out
gcc demos/demo2.out.s helper.s -o demos/demo2.run
./demos/demo2.run

rm demos/demo2.out
rm demos/demo2.out.s
rm demos/demo2.run
