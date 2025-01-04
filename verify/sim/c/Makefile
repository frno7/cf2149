# SPDX-License-Identifier: GPL-2.0

VERIFY_CFLAGS = $(BASIC_HOST_CFLAGS) $(HOST_CFLAGS)

SIM_C_SRC := verify/sim/c/sim.c
SIM_C_OBJ := $(SIM_C_SRC:%.c=%.o)
SIM_C := verify/sim/c/sim

ALL_OBJ += $(SIM_C_OBJ)

$(SIM_C_OBJ): %.o : %.c
	$(QUIET_CC)$(HOST_CC) $(VERIFY_CFLAGS) -c -o $@ $<

$(SIM_C): $(SIM_C_OBJ) $(CF2149_OBJ)
	$(QUIET_LINK)$(HOST_CC) $(VERIFY_CFLAGS) -o $@ $^

VERIFY_C_SVS := $(VERIFY_PVS:%.pvs=%-c.svs)
VERIFY_C_VCD := $(VERIFY_PVS:%.pvs=%-c.vcd)

$(VERIFY_C_SVS): $(SIM_C)
$(VERIFY_C_SVS): %-c.svs: %.pvs
	$(QUIET_SIM)$(SIM_C) $< $@

$(VERIFY_C_VCD): $(TOOL_HS)
$(VERIFY_C_VCD): %.vcd: %.svs
	$(QUIET_SIM)$(TOOL_SVS) vcd $< $@

OTHER_CLEAN += $(SIM_C) $(VERIFY_C_SVS) $(VERIFY_C_VCD)
