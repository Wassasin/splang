#!/bin/bash

splang --target=llvm mergesort.spl | opt -O3 | llc -filetype=obj > mergesort.o
splang --target=llvm insertionsort.spl | opt -O3 | llc -filetype=obj > insertionsort.o
splang --target=llvm main.spl | opt -O3 | llc -filetype=obj > main.o

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   llvm-ld -native mergesort.o insertionsort.o main.o
elif [[ "$unamestr" == 'Darwin' ]]; then
   ld /usr/lib/libc.dylib /usr/lib/crt1.o mergesort.o insertionsort.o main.o
fi

./a.out
