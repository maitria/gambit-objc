
test_PROGRAMS		= $(basename $(wildcard *-test.scm))

.PHONY: test
test: clean $(test_PROGRAMS)
	for t in $(test_PROGRAMS); do printf '%s: ' "$$t"; ./$$t || exit $$?; done

%-test: %-test.scm
	bh exe $<

.PHONY: clean
clean:
	rm -f *.o[0-9] *.o[0-9].deps $(test_PROGRAMS)
