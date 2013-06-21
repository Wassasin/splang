#!/bin/bash

splang --target=llvm shuffle.spl | opt -O3 | llc -filetype=obj > shuffle.o
clang -c -o get_int.o get_int.c

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   llvm-ld -native shuffle.o get_int.o
elif [[ "$unamestr" == 'Darwin' ]]; then
   ld /usr/lib/libc.dylib /usr/lib/crt1.o shuffle.o get_int.o
fi

./a.out
