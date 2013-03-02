
CC		= gcc-4.2
CFLAGS		= -x objective-c -lobjc -lgambc -framework Foundation

BUILD_DIR	= build

lib_SOURCES	= $(filter-out %\#.scm,$(wildcard lib/*.scm))
lib_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(lib_SOURCES))))

test_SOURCES	= $(filter-out %\#.scm,$(wildcard test/*.scm))
test_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(test_SOURCES))))
test_PROGRAMS	= $(basename $(test_SOURCES))

.PHONY: test
test: $(test_PROGRAMS)
	@for t in $(test_PROGRAMS); do printf '            TEST %s ... ' "$$t"; ./$$t || exit $$?; done

$(BUILD_DIR)/%.c: %.scm
	@printf '         COMPILE $^\n'
	@mkdir -p $(dir $@)
	@gsc -c -o $@ $(gsc_FLAGS) $^

%_.c: $(lib_CFILES) %.c
	@printf '  MAKE-LINK-FILE $@\n'
	@gsc -link -o $@ $(gsc_FLAGS) $^

test/%: $(lib_CFILES) $(BUILD_DIR)/test/%.c $(BUILD_DIR)/test/%_.c
	@printf '    MAKE-PROGRAM $@\n'
	@$(CC) $(CFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/* $(test_PROGRAMS)
