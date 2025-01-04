# SPDX-License-Identifier: GPL-2.0

VERIFY_PCS := $(wildcard verify/bus/*.pcs)
VERIFY_PVS := $(VERIFY_PCS:%.pcs=%.pvs)

$(VERIFY_PVS): $(TOOL_HS)

$(VERIFY_PVS): %.pvs: %.pcs
	$(QUIET_GEN)$(TOOL_PCS) pvs $< $@

OTHER_CLEAN += $(VERIFY_PVS)
