#!/bin/bash

# opam install llvm
# brew install llvm

# /usr/local/opt/llvm/bin/llc="llc"

eval $(opam env)
rm -rf _build
ocamlbuild -pkgs llvm serene.native
gcc -S helper.c

./serene.native -l tests/example.sn > tests/example.out
/usr/local/opt/llvm/bin/llc tests/example.out
gcc tests/example.out.s helper.s -o tests/example.run
./tests/example.run

./serene.native -l tests/while.sn > tests/while.out
/usr/local/opt/llvm/bin/llc tests/while.out
gcc tests/while.out.s helper.s -o tests/while.run
./tests/while.run

./serene.native -l tests/graph.sn > tests/graph.out
/usr/local/opt/llvm/bin/llc tests/graph.out
gcc tests/graph.out.s helper.s -o tests/graph.run
./tests/graph.run

./serene.native -l tests/if.sn > tests/if.out
/usr/local/opt/llvm/bin/llc tests/if.out
gcc tests/if.out.s helper.s -o tests/if.run
./tests/if.run

./serene.native -l tests/ifelse.sn > tests/ifelse.out
/usr/local/opt/llvm/bin/llc tests/ifelse.out
gcc tests/ifelse.out.s helper.s -o tests/ifelse.run
./tests/ifelse.run

./serene.native -l tests/factorial.sn > tests/factorial.out
/usr/local/opt/llvm/bin/llc tests/factorial.out
gcc tests/factorial.out.s helper.s -o tests/factorial.run
./tests/factorial.run

./serene.native -l tests/fibonacci.sn > tests/fibonacci.out
/usr/local/opt/llvm/bin/llc tests/fibonacci.out
gcc tests/fibonacci.out.s helper.s -o tests/fibonacci.run
./tests/fibonacci.run

./serene.native -l tests/foo_bar.sn > tests/foo_bar.out
/usr/local/opt/llvm/bin/llc tests/foo_bar.out
gcc tests/foo_bar.out.s helper.s -o tests/foo_bar.run
./tests/foo_bar.run

./serene.native -l tests/operations.sn > tests/operations.out
/usr/local/opt/llvm/bin/llc tests/operations.out
gcc tests/operations.out.s helper.s -o tests/operations.run
./tests/operations.run

./serene.native -l tests/reverse.sn > tests/reverse.out
/usr/local/opt/llvm/bin/llc tests/reverse.out
gcc tests/reverse.out.s helper.s -o tests/reverse.run
./tests/reverse.run

./serene.native -l tests/list_of_lists.sn > tests/list_of_lists.out
/usr/local/opt/llvm/bin/llc tests/list_of_lists.out
gcc tests/list_of_lists.out.s helper.s -o tests/list_of_lists.run
./tests/list_of_lists.run

./serene.native -l tests/external_calls.sn > tests/external_calls.out
/usr/local/opt/llvm/bin/llc tests/external_calls.out
gcc tests/external_calls.out.s helper.s -o tests/external_calls.run
./tests/external_calls.run

./serene.native -l tests/findMax.sn > tests/findMax.out
/usr/local/opt/llvm/bin/llc tests/findMax.out
gcc tests/findMax.out.s helper.s -o tests/findMax.run
./tests/findMax.run

./serene.native -l tests/findMin.sn > tests/findMin.out
/usr/local/opt/llvm/bin/llc tests/findMin.out
gcc tests/findMin.out.s helper.s -o tests/findMin.run
./tests/findMin.run

./serene.native -l tests/uppercase.sn > tests/uppercase.out
/usr/local/opt/llvm/bin/llc tests/uppercase.out
gcc tests/uppercase.out.s helper.s -o tests/uppercase.run
./tests/uppercase.run

./serene.native -l tests/lowercase.sn > tests/lowercase.out
/usr/local/opt/llvm/bin/llc tests/lowercase.out
gcc tests/lowercase.out.s helper.s -o tests/lowercase.run
./tests/lowercase.run

./serene.native -l tests/reversestr.sn > tests/reversestr.out
/usr/local/opt/llvm/bin/llc tests/reversestr.out
gcc tests/reversestr.out.s helper.s -o tests/reversestr.run
./tests/reversestr.run

./serene.native -l tests/random.sn > tests/random.out
/usr/local/opt/llvm/bin/llc tests/random.out
gcc tests/random.out.s helper.s -o tests/random.run
./tests/random.run

./serene.native -l tests/gcd.sn > tests/gcd.out
/usr/local/opt/llvm/bin/llc tests/gcd.out
gcc tests/gcd.out.s helper.s -o tests/gcd.run
./tests/gcd.run

./serene.native -l tests/rangewithstep.sn > tests/rangewithstep.out
/usr/local/opt/llvm/bin/llc tests/rangewithstep.out
gcc tests/rangewithstep.out.s helper.s -o tests/rangewithstep.run
./tests/rangewithstep.run

./serene.native -l tests/range.sn > tests/range.out
/usr/local/opt/llvm/bin/llc tests/range.out
gcc tests/range.out.s helper.s -o tests/range.run
./tests/range.run

./serene.native -l tests/isprime.sn > tests/isprime.out
/usr/local/opt/llvm/bin/llc tests/isprime.out
gcc tests/isprime.out.s helper.s -o tests/isprime.run
./tests/isprime.run

find . -type f -name '*.out' -delete
find . -type f -name '*.s' -delete
find . -type f -name '*.run' -delete