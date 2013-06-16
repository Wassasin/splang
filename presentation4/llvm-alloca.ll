; "Updating" a local variable requires alloca
define i32 @pow(i32 %ibase, i32 %iexp){
cp:	%exp = alloca i32
	store i32 %iexp, i32* %exp
	%acc = alloca i32
	store i32 1, i32* %acc
	br label %t
t:	%0 = load i32* %exp
	%1 = icmp eq i32 %0, 0
	br i1 %1, label %done, label %calc
calc:	%2 = load i32* %acc
	%3 = mul i32 %ibase, %2
	store i32 %3, i32* %acc
	%4 = sub i32 %0, 1
	store i32 %4, i32* %exp
	br label %t
done:	%r = load i32* %acc
	ret i32 %r
}
