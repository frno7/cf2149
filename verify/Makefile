# SPDX-License-Identifier: GPL-2.0

include verify/tool/Makefile
include verify/bus/Makefile

VERIFY_PVS_BASE:= $(VERIFY_PVS:%.pvs=%)

include verify/sim/Makefile

VERIFY_VCD := $(VERIFY_C_VCD)

.PHONY: pvs
pvs: $(VERIFY_PVS)

.PHONY: vcd
vcd: $(VERIFY_VCD)

.PHONY: verify
verify: vcd
