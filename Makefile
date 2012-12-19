test: test.scm expect.scm objc.scm test-methods.m
	gsc -o test -exe -ld-options "-lobjc -framework Foundation" test.scm test-methods.m
	./test
