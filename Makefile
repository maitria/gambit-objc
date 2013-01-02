
.PHONY: test
test:
	bh exe objc-test.scm
	./objc-test

.PHONY: clean
clean:
	rm -f *.o[0-9] *.o[0-9].deps objc-test
