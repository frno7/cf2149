# SPDX-License-Identifier: GPL-2.0

ifdef BUILD_COMPILE
BUILD_CC = $(BUILD_COMPILE)gcc
else
BUILD_CC = $(CC)
endif

ifdef HOST_COMPILE
HOST_CC = $(HOST_COMPILE)gcc
HOST_AR = $(HOST_COMPILE)ar
else
HOST_CC = $(CC)
HOST_AR = $(AR)
endif

ifdef TARGET_COMPILE
TARGET_CC = $(TARGET_COMPILE)gcc
TARGET_LD = $(TARGET_COMPILE)ld
TARGET_AR = $(TARGET_COMPILE)ar
endif

CFLAGS = -g
BUILD_CFLAGS = $(CFLAGS)
HOST_CFLAGS = $(CFLAGS)
TARGET_CFLAGS = $(CFLAGS)

ifeq (1,$(S))
S_CFLAGS = -fsanitize=address -fsanitize=leak -fsanitize=undefined	\
	  -fsanitize-address-use-after-scope -fstack-check		\
	  -fno-stack-clash-protection
endif

DEP_CFLAGS = -Wp,-MD,$(@D)/$(@F).d -MT $(@D)/$(@F)
BASIC_CFLAGS = -O2 -Wall -D_GNU_SOURCE $(HAVE_CFLAGS) $(DEP_CFLAGS)
BASIC_BUILD_CFLAGS = -Iinclude $(S_CFLAGS) $(BASIC_CFLAGS)
BASIC_HOST_CFLAGS = -Iinclude $(S_CFLAGS) $(BASIC_CFLAGS)
BASIC_TARGET_CFLAGS = -Iinclude $(BASIC_CFLAGS)

.PHONY: all
all:

include module/Makefile
include verify/Makefile

.PHONY: module
module: $(CF2149_OBJ)

ALL_DEP = $(sort $(ALL_OBJ:%=%.d))

all: $(CF2149_OBJ) $(CF2149_ATARI_ST_OBJ)

.PHONY: gtags
gtags:
	$(QUIET_GEN)gtags
OTHER_CLEAN += GPATH GRTAGS GTAGS

.PHONY: clean
clean:
	$(QUIET_RM)$(RM) $(ALL_OBJ) $(ALL_DEP) $(OTHER_CLEAN)

V              = @
Q              = $(V:1=)
QUIET_AR       = $(Q:@=@echo    '  AR       '$@;)
QUIET_AS       = $(Q:@=@echo    '  AS       '$@;)
QUIET_CC       = $(Q:@=@echo    '  CC       '$@;)
QUIET_VHDLC    = $(Q:@=@echo    '  VHDLC    '$@;)
QUIET_VERILOGC = $(Q:@=@echo    '  VERILOGC '$@;)
QUIET_SIM      = $(Q:@=@echo    '  SIM      '$@;)
QUIET_GEN      = $(Q:@=@echo    '  GEN      '$@;)
QUIET_LINK     = $(Q:@=@echo    '  LD       '$@;)
QUIET_RM       = $(Q:@=@echo    '  RM       '$@;)
QUIET_CHECK    = $(Q:@=@echo    '  CHECK    '$@;)
QUIET_TEST     = $(Q:@=@echo    '  TEST     '$@;)

$(eval -include $(ALL_DEP))
