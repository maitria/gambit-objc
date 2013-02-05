
test_PROGRAMS		= objc-test \
			  objc-x86_64-test \
			  x86_64-trampoline-test \
			  x86_64-load-test


.PHONY: test
test: clean $(test_PROGRAMS)
	for t in $(test_PROGRAMS); do printf '%s: ' "$$t"; ./$$t || exit $$?; done

%-test: %-test.scm
	bh exe $<

.PHONY: clean
clean:
	rm -f *.o[0-9] *.o[0-9].deps $(test_PROGRAMS)
