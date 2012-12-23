test: test.scm objc.c test.c test-methods.m
	gsc -o test -exe -ld-options "-lobjc -framework Foundation" objc test test-methods.m
	./test

objc.c: objc.scm objc\#.scm
	gsc -c objc

test.c: test.scm expect.scm
	gsc -c test
