
test: test.scm objc.scm test-methods.m
	gsc -:s -o test -exe -ld-options "-lobjc -framework Foundation" test.scm test-methods.m
	./test
