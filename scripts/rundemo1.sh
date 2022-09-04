#!/bin/bash

# opam install llvm
# brew install llvm

eval $(opam env)
rm -rf _build

ocamlbuild -pkgs llvm serene.native
gcc -S helper.c

./serene.native -l demos/demo1.sn > demos/demo1.out
/usr/local/opt/llvm/bin/llc demos/demo1.out
gcc demos/demo1.out.s helper.s -o demos/demo1.run
./demos/demo1.run
rm demos/demo1.out
rm demos/demo1.out.s
rm demos/demo1.run
