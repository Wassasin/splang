#!/bin/bash

splang --target=llvm sum.spl | opt -O3 | llc -filetype=obj > sum.o
clang++ -std=c++11 -c -o main.o main.cpp

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   ld -native shuffle.o get_int.o
elif [[ "$unamestr" == 'Darwin' ]]; then
   ld /usr/lib/libc.dylib /usr/lib/libstdc++.dylib /usr/lib/crt1.o sum.o main.o
fi

./a.out
