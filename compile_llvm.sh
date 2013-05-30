#!/bin/bash

# lli is the LLVM interpreter
# opt is the LLVM optimizer (transformation on LLVM IR)
# llc is the LLVM compiler (to assembly)
# ld is the gnu linker, needs libc for malloc/printf/etc, needs crt1 for start

f=$1
cat $1 | opt -S | tee $f.opt | llc -filetype=obj > $f.o || exit 1
ld $f.o /usr/lib/libc.dylib /usr/lib/crt1.o || exit 1
