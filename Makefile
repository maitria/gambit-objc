
CC		= gcc-4.2
CFLAGS		= -x objective-c
LDFLAGS		= -lobjc -lgambc -framework Foundation

BUILD_DIR	= build

lib_SOURCES	= $(filter-out %\#.scm,$(wildcard lib/*.scm))
lib_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(lib_SOURCES))))
lib_OBJECTS	= $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(basename $(lib_SOURCES))))

test_SOURCES	= $(filter-out %\#.scm,$(wildcard test/*.scm))
test_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(test_SOURCES))))
test_OBJECTS	= $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(basename $(test_SOURCES))))
test_LINKFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix _.c,$(basename $(test_SOURCES))))
test_LINKOBJECTS= $(addprefix $(BUILD_DIR)/,$(addsuffix _.o,$(basename $(test_SOURCES))))
test_PROGRAMS	= $(basename $(test_SOURCES))

test: $(test_PROGRAMS)
	@for t in $(test_PROGRAMS); do printf '            TEST %s ... ' "$$t"; ./$$t || exit $$?; done

$(BUILD_DIR)/%.c: %.scm
	@printf '         COMPILE $^\n'
	@mkdir -p $(dir $@)
	@gsc -c -o $@ $(gsc_FLAGS) $^

%_.c: $(lib_CFILES) %.c
	@printf '  MAKE-LINK-FILE $@\n'
	@gsc -link -o $@ $(gsc_FLAGS) $^

%.o: %.c
	@printf '       COMPILE-C $^\n'
	@$(CC) -c $(CFLAGS) -o $@ $^

test/%: $(lib_OBJECTS) $(BUILD_DIR)/test/%.o $(BUILD_DIR)/test/%_.o
	@printf '    MAKE-PROGRAM $@\n'
	@$(CC) $(LDFLAGS) -o $@ $^

clean:
	rm -rf $(BUILD_DIR)/* $(test_PROGRAMS)

.PHONY: test clean
.SECONDARY: $(lib_CFILES) $(test_CFILES) $(lib_OBJECTS) $(test_OBJECTS) $(test_LINKFILES) $(test_LINKOBJECTS)
