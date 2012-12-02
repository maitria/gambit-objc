
test: test.scm objc.scm call-method.c
	gsc -:s -o test -exe -ld-options "-lobjc -framework Foundation" test.scm
	./test
