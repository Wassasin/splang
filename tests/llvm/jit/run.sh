#!/bin/bash

# sorry for the hard coded paths...

llvm_install_path="/Users/joshua/Documents/Code/llvm-3.3"
llvm_libs=`$llvm_install_path/bin/llvm-config --libs`
llvm_cxxflags="-I/Users/joshua/Documents/Code/llvm-3.3/include  -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -O3  -stdlib=libc++ -std=c++11 -fvisibility-inlines-hidden -fno-rtti -fno-common -Woverloaded-virtual -Wcast-qual"

libcxx_path="/Users/joshua/Documents/Code/libcxx"

splang --target=llvm program.spl > program.ll

$CXX $llvm_cxxflags -O3 -isystem"$libcxx_path/include" -isystem"$llvm_install_path/include" $llvm_libs -o kareltje main.cpp

./kareltje
