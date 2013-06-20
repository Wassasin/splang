#!/bin/bash

# Will time how fast we can calculate fibonacci!
# Output is send to /dev/null, so that we only see the time

# Ok, don't do ackermann!!!
for f in fib ackermann; do
	# compile the spl file to SSM
	splang --target=ssm $f.spl > $f.ssm

	# Run it with time
	echo -e "\n\033[32m$f SSM\033[0m"
	if [[ $f == 'fib' ]]; then
		time java -jar ../../../../ssm-nogui/build/ssm-nogui.jar $f.ssm 1>/dev/null
	else
		echo "Computation skipped, because it would take too long ;)"
	fi

	# compile to LLVM IR and interpret
	splang --target=llvm $f.spl > $f.ll
	echo -e "\n\033[32m$f LLVM IR Interpreter\033[0m"
	time lli $f.ll 1>/dev/null

	# ... and generate native code
	llc -filetype=obj $f.ll > $f.o
	if [[ `uname` == 'Darwin' ]]; then
		ld /usr/lib/libc.dylib /usr/lib/crt1.o $f.o
	else
		llvm-ld -native $f.o
	fi
	echo -e "\n\033[32m$f LLVM IR native code\033[0m"
	time ./a.out 1>/dev/null

	# ... and generate optimized code
	opt -O3 $f.ll | llc -filetype=obj > $f.o
	if [[ `uname` == 'Darwin' ]]; then
		ld /usr/lib/libc.dylib /usr/lib/crt1.o $f.o
	else
		llvm-ld -native $f.o
	fi
	echo -e "\n\033[32m$f LLVM IR optimized native code\033[0m"
	time ./a.out 1>/dev/null
done
