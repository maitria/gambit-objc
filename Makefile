
test: test.scm objc.scm
	gsc -:s -o test -exe -ld-options "-lobjc -framework Foundation" test.scm
	./test
