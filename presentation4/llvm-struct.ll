; swap :: (Int, Bool) -> (Bool, Int)
define {i1, i32} @swap({i32, i1} %arg){
entry:
	%0 = extractvalue {i32, i1} %arg, 0
	%1 = extractvalue {i32, i1} %arg, 1
	%2 = insertvalue {i1, i32} undef, i32 %0, 1
	%3 = insertvalue {i1, i32} %2, i1 %1, 0
	ret {i1, i32} %3
}
