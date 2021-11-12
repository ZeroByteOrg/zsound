# Makefile adapted from general-purpose Makefile by Job Vranish
# "Even simpler Makefile"
# https://spin.atomicobject.com/2016/08/26/makefile-c-projects/

REV		= 38

LIBDIR	?= lib
LIBRARY ?= $(LIBDIR)/zsound.lib


EXEC ?= TEST.PRG
SRC_DIRS ?= ./src

CC		= cl65
AS		= cl65
LD		= cl65
AR		= ar65

FLAGS		= -t cx16 -g
CFLAGS		= $(FLAGS) -O $(INC_FLAGS)
ASFLAGS		= $(FLAGS) -c
LDFLAGS		= $(FLAGS) -C zsound.cfg -u __EXEHDR__ -o
REV38		= --asm-define REV=38
REV39		= --asm-define REV=39

SRCS := $(shell find $(SRC_DIRS) -name \*.asm)
#SRCS := $(shell find $(SRC_DIRS) -name \*.c)
OBJLIST := $(addsuffix .o,$(basename $(SRCS)))
OBJ38	:= $(addsuffix 38,$(OBJLIST))
OBJ39	:= $(addsuffix 39,$(OBJLIST))
OBJS	:= $(OBJ38) $(OBJ39)
#OBJS	:= $(OBJLIST)
DEPS := $(OBJLIST:.o=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

$(LIBRARY): $(OBJS)
	@[ -d $(LIBDIR) ] || mkdir -p $(LIBDIR)
	$(AR) a $@ $(OBJS)

.PHONY: lib
lib: $(LIBRARY)

src/%.o38: src/%.asm
	$(AS) $(ASFLAGS) $(REV38) -o $@ $<

src/%.o39: src/%.asm
	$(AS) $(ASFLAGS) $(REV39) -o $@ $<

.PHONY: clean
clean:
	$(RM) $(EXEC) $(LIBRARY) $(OBJS) $(DEPS) $(SYM) $(SYMS)

.PHONY: objclean
objclean:
	$(RM) $(OBJS)
	
.PHONY: test
test: $(EXEC)

.PHONY: %.h

.PHONY: %.inc

-include $(DEPS)
