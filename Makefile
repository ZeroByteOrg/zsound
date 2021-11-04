# Makefile adapted from general-purpose Makefile by Job Vranish
# "Even simpler Makefile"
# https://spin.atomicobject.com/2016/08/26/makefile-c-projects/

REV		= 38

LIB38	?= lib/zsound38.lib
LIB39	?= lib/zsound39.lib
LIBRARY ?= lib/zsound$(REV).lib


EXEC ?= TEST.PRG
SRC_DIRS ?= ./src

CC		= /usr/local/bin/cl65
AS		= /usr/local/bin/cl65
LD		= /usr/local/bin/cl65
AR		= /usr/local/bin/ar65

FLAGS		= -t cx16 -g
CFLAGS		= $(FLAGS) -O $(INC_FLAGS)
ASFLAGS		= $(FLAGS) -c
LDFLAGS		= $(FLAGS) -C zsound.cfg -u __EXEHDR__ -o
REVFLAGS	= --asm-define REV=$(REV)

SRCS := $(shell find $(SRC_DIRS) -name \*.asm)
#SRCS := $(shell find $(SRC_DIRS) -name \*.c)
OBJLIST := $(addsuffix .o,$(basename $(SRCS)))
OBJ38	:= $(addprefix $(BUILDDIR)/r38_,$(OBJLIST))
OBJ39	:= $(addprefix $(BUILDDIR)/r39_,$(OBJLIST))
OBJS	:= $(OBJ38) $(OBJ39)
DEPS := $(OBJLIST:.o=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

$(LIBRARY): $(OBJS)
	$(AR) a $@ $(OBJS)

.PHONY: lib
lib: $(LIBRARY)
#	REV	=	38
#	make $(LIB38)
#	REV	=	39
#	make $(LIB39)

src/%.o: src/%.asm
	$(AS) $(ASFLAGS) $(REVFLAGS) -o $@ $<

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
