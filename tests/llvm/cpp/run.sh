#!/bin/bash

splang --target=llvm sum.spl | opt -O3 | llc -filetype=obj > sum.o
clang++ -std=c++11 -c -o main.o main.cpp

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   llvm-ld -native sum.o main.o /usr/lib/x86_64-linux-gnu/libstdc++.so.6
elif [[ "$unamestr" == 'Darwin' ]]; then
   ld /usr/lib/libc.dylib /usr/lib/libstdc++.dylib /usr/lib/crt1.o sum.o main.o
fi

./a.out
