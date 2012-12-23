
.PHONY: test
test: objc-test
	./objc-test

objc-test: objc.c objc-test.c test-methods.m
	gsc -o objc-test -exe -ld-options "-lobjc -framework Foundation" objc objc-test test-methods.m

objc.c: objc.scm objc\#.scm
	gsc -c objc

objc-test.c: objc-test.scm expect.scm
	gsc -c objc-test
