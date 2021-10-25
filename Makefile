# Makefile adapted from general-purpose Makefile by Job Vranish
# "Even simpler Makefile"
# https://spin.atomicobject.com/2016/08/26/makefile-c-projects/

LIBRARY ?= lib/zsound.lib
EXEC ?= TEST.PRG
SRC_DIRS ?= ./src

CC		= /usr/local/bin/cl65
AS		= /usr/local/bin/cl65
LD		= /usr/local/bin/cl65
AR		= /usr/local/bin/ar65

FLAGS	= -t cx16
CFLAGS	= $(FLAGS) -O $(INC_FLAGS)
ASFLAGS	= $(FLAGS) -c -o
LDFLAGS	= $(FLAGS) -C zsound.cfg -u __EXEHDR__ -o

SRCS := $(shell find $(SRC_DIRS) -name \*.asm)
#SRCS := $(shell find $(SRC_DIRS) -name \*.c)
OBJS := $(addsuffix .o,$(basename $(SRCS)))
DEPS := $(OBJS:.o=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

$(LIBRARY): $(OBJS)
	$(AR) a $(LIBRARY) $(OBJS)
	
src/%.o: src/%.asm
	$(AS) $(ASFLAGS) $@ $<

.PHONY: clean
clean:
	$(RM) $(EXEC) $(LIBRARY) $(OBJS) $(DEPS) $(SYM) $(SYMS)
	
.PHONY: lib
lib: $(LIBRARY)

.PHONY: test
test: $(EXEC)

.PHONY: %.h

.PHONY: %.inc

-include $(DEPS)


