
CC		= gcc-4.2
CFLAGS		= -g -x objective-c
LDFLAGS		= -g -lobjc -lgambc -lffi -framework Foundation

bin/cocoa-example: LDFLAGS += -framework Cocoa

BUILD_DIR	= build

lib_SOURCES	= $(filter-out %\#.scm,$(wildcard lib/*.scm))
lib_C_SOURCES	= $(wildcard lib/*.c)
lib_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(lib_SOURCES))))
lib_OBJECTS	= $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(basename $(lib_SOURCES)))) \
		  $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(basename $(lib_C_SOURCES))))

test_SOURCES	= $(filter-out %\#.scm,$(wildcard test/*.scm))
test_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(test_SOURCES))))
test_OBJECTS	= $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(basename $(test_SOURCES))))
test_LINKFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix _.c,$(basename $(test_SOURCES))))
test_LINKOBJECTS= $(addprefix $(BUILD_DIR)/,$(addsuffix _.o,$(basename $(test_SOURCES))))
test_PROGRAMS	= $(basename $(test_SOURCES))

bin_SOURCES	= $(filter-out %\#.scm,$(wildcard bin/*.scm))
bin_CFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix .c,$(basename $(bin_SOURCES))))
bin_OBJECTS	= $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(basename $(bin_SOURCES))))
bin_LINKFILES	= $(addprefix $(BUILD_DIR)/,$(addsuffix _.c,$(basename $(bin_SOURCES))))
bin_LINKOBJECTS	= $(addprefix $(BIULD_DIR)/,$(addsuffix _.o,$(basename $(bin_SOURCES))))
bin_PROGRAMS	= $(basename $(bin_SOURCES))

all: test $(bin_PROGRAMS)

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

$(BUILD_DIR)/%.o: %.c $(word 1,$(lib_CFILES))
	@printf '       COMPILE-C $^\n'
	@$(CC) -c $(CFLAGS) -D___VERSION="`sed -n 's/#define *___VERSION //p' $(word 1,$(lib_CFILES))`" -o $@ $<

test/%: $(lib_OBJECTS) $(BUILD_DIR)/test/%.o $(BUILD_DIR)/test/%_.o
	@printf '    MAKE-PROGRAM $@\n'
	@$(CC) $(LDFLAGS) -o $@ $^

bin/%: $(lib_OBJECTS) $(BUILD_DIR)/bin/%.o $(BUILD_DIR)/bin/%_.o
	@printf '    MAKE-PROGRAM $@\n'
	@$(CC) $(LDFLAGS) -o $@ $^

clean:
	rm -rf $(BUILD_DIR)/* $(test_PROGRAMS)

.PHONY: test clean
.SECONDARY: $(lib_CFILES) $(test_CFILES) $(lib_OBJECTS) $(test_OBJECTS) \
	$(test_LINKFILES) $(test_LINKOBJECTS)
