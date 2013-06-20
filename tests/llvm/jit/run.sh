#!/bin/bash

# sorry for the hard coded paths...

llvm_install_path="/Users/joshua/Documents/Code/llvm-3.3"
llvm_libs=`$llvm_install_path/bin/llvm-config --libs`
llvm_cxxflags="-I$llvm_install_path/include  -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -O3  -stdlib=libc++ -std=c++11 -fvisibility-inlines-hidden -fno-rtti -fno-common -Woverloaded-virtual -Wcast-qual"

libcxx_path="/Users/joshua/Documents/Code/libcxx"

boost_process_path="/Users/joshua/Documents/Code/process/"
boost_libs="-lboost_iostreams -lboost_system"

$CXX $llvm_cxxflags -O3 -isystem"$libcxx_path/include" -isystem"$llvm_install_path/include" -isystem"$boost_process_path" $boost_libs $llvm_libs -o kareltje main.cpp

./kareltje
