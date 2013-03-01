
CC		= gcc-4.2
CFLAGS		= -x objective-c -lobjc -lgambc -framework Foundation

lib_SOURCES	= $(filter-out %\#.scm,$(wildcard lib/*.scm))
lib_CFILES	= $(addsuffix .c,$(basename $(lib_SOURCES)))

test_SOURCES	= $(filter-out %\#.scm,$(wildcard test/*.scm))
test_CFILES	= $(addsuffix .c,$(basename $(test_SOURCES)))
test_PROGRAMS	= $(basename $(test_SOURCES))

.PHONY: test
test: $(test_PROGRAMS)
	for t in $(test_PROGRAMS); do printf '%s: ' "$$t"; ./$$t || exit $$?; done

%.c: %.scm
	gsc -c -o $@ $(gsc_FLAGS) $^

test/%_.c: $(lib_CFILES) test/%.c
	gsc -link -o $@ $(gsc_FLAGS) $^

test/%: $(lib_CFILES) test/%.c test/%_.c
	$(CC) $(CFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -f $(test_PROGRAMS) $(lib_CFILES) $(lib_LIBRARY)
