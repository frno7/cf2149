// SPDX-License-Identifier: GPL-2.0
/* Copyright (C) 2025 Fredrik Noring */

#define _POSIX_C_SOURCE 1

#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cf2149/assert.h"
#include "cf2149/macro.h"
#include "cf2149/types.h"

#include "cf2149/module/assert.h"
#include "cf2149/module/cf2149.h"

#define ERROR_LEVEL MODULE_ERR

#define PVS_WIDTH    95
#define CLOCK_CYCLE 500		/* 500 ns clock cycle for 2 MHz */

static bool error;

static void report(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);

	if (fmt[0] == MODULE_SOH[0]) {
		if (fmt[1] <= (ERROR_LEVEL)[1])
			error = true;
		fmt = &fmt[fmt[1] ? 2 : 1];
	}

	vfprintf(stderr, fmt, ap);

	va_end(ap);
}

static uint64_t from_binary(int length, const char *b)
{
	uint64_t v = 0;

	for (int i = 0; i < length; i++)
		v |= (uint64_t)(b[i] == '1') << (length - 1 - i);

	return v;
}

static void svs_entry(FILE *svs_file,
	uint64_t cycle, const struct cf2149_module *cf2149, struct cf2149_ac ac)
{
	static struct buf { char s[4096]; } a, b;

	snprintf(a.s, sizeof(a.s),
		" port_reset_l.1=%d"
		" port_select_l.1=%d"
		" port_bdir.1=%d"
		" port_bc2.1=%d"
		" port_bc1.1=%d"
		" ac_lva.8=%d" " ac_lvb.8=%d" " ac_lvc.8=%d"
		" reg_select.4=%d"
		" reg_00_plo_a.8=%d" " reg_01_phi_a.4=%d"
		" reg_02_plo_b.8=%d" " reg_03_phi_b.4=%d"
		" reg_04_plo_c.8=%d" " reg_05_phi_c.4=%d"
		" reg_06_noise.5=%d"
		" reg_07_iomix.8=x%x"
		" reg_08_level_a.5=x%x"
		" reg_09_level_b.5=x%x"
		" reg_10_level_c.5=x%x"
		" reg_11_plo_env.8=x%x" " reg_12_phi_env.8=x%x"
		" reg_13_shape.4=x%x"
		" reg_14_io_a.8=x%x" " reg_15_io_b.8=x%x"
		" noise_lfsr.17=x%x"
		" env_p.8=x%x" " env_wave.8=x%x",
		cf2149->port.state.reset_l,
		cf2149->port.state.select_l,
		cf2149->port.state.bdc.bdir,
		cf2149->port.state.bdc.bc2,
		cf2149->port.state.bdc.bc1,
		ac.lva.b8, ac.lvb.b8, ac.lvc.b8,
		cf2149->state.reg_address,
		cf2149->state.reg.u8[ 0], cf2149->state.reg.u8[ 1],
		cf2149->state.reg.u8[ 2], cf2149->state.reg.u8[ 3],
		cf2149->state.reg.u8[ 4], cf2149->state.reg.u8[ 5],
		cf2149->state.reg.u8[ 6],
		cf2149->state.reg.u8[ 7],
		cf2149->state.reg.u8[ 8],
		cf2149->state.reg.u8[ 9],
		cf2149->state.reg.u8[10],
		cf2149->state.reg.u8[11], cf2149->state.reg.u8[12],
		cf2149->state.reg.u8[13],
		cf2149->state.reg.u8[14], cf2149->state.reg.u8[15],
		cf2149->state.noise.lfsr,
		cf2149->state.env.p, cf2149->state.env.wave);

	if (!strcmp(a.s, b.s))
		return;

	strcpy(b.s, a.s);

	fprintf(svs_file, "/c/cf2149 # %4" PRIu64 " @ %d ns: "
			  "cycle.64=%" PRIu64 " %s\n",
		cycle, CLOCK_CYCLE, cycle, a.s);
}

int main(int argc, char *argv[])
{
	struct cf2149_module cf2149 = cf2149_init();

	if (argc != 3) {
		fprintf(stderr, "usage: sim <pvs-file> <svs-file>\n");

		return EXIT_FAILURE;
	}

	const char * const psb_path = argv[1];
	const char * const svs_path = argv[2];
	char tmp_path[PATH_MAX];

	snprintf(tmp_path, sizeof(tmp_path), "%s.tmp", svs_path);

	FILE *psb_file = fopen(psb_path, "r");
	if (!psb_file) {
		perror(psb_path);
		return EXIT_FAILURE;
	}

	FILE *svs_file = fopen(tmp_path, "wt");
	if (!svs_file) {
		perror(svs_path);
		return EXIT_FAILURE;
	}

	uint64_t cycle = 0;
	char *line = NULL;
	size_t length = 0;
	struct port {
		uint64_t cycle;
		bool     reset_l;
		/* Data/address bus */
		uint8_t  i_da;
		/* Bus control */
		bool     i_a9_l;
		bool     i_a8;
		bool     i_bdir;
		bool     i_bc2;
		bool     i_bc1;
		/* Clock divisor control */
		bool     i_sel_l;
		/* Port A */
		uint8_t  i_oa;
		/* Port B */
		uint8_t  i_ob;
	} port = { };
	struct cf2149_ac ac[1] = { };

	cf2149.debug.report = report;

	while (getline(&line, &length, psb_file) != -1) {
		if (strlen(line) != PVS_WIDTH + 1) {
			fprintf(stderr, "Malformed line %zu: %s\n",
				strlen(line), line);
			goto err;
		}

		port.cycle = from_binary(64, &line[0]);
		while (cycle < port.cycle) {
			svs_entry(svs_file, cycle, &cf2149, ac[0]);

			cycle++;

			cf2149.port.rd_ac(&cf2149, cf2149_clk_cycle(cycle),
				ac, ARRAY_SIZE(ac));
		}

		port = (struct port) {
			.reset_l  = from_binary(1, &line[64]),
			.i_sel_l  = from_binary(1, &line[65]),
			.i_bdir   = from_binary(1, &line[66]),
			.i_bc2    = from_binary(1, &line[67]),
			.i_bc1    = from_binary(1, &line[68]),
			.i_a9_l   = from_binary(1, &line[69]),
			.i_a8     = from_binary(1, &line[70]),
			.i_da     = from_binary(8, &line[71]),
			.i_oa     = from_binary(8, &line[79]),
			.i_ob     = from_binary(8, &line[87]),
		};

		cf2149.port.reset_l(&cf2149, cf2149_clk_cycle(cycle), port.reset_l);
		cf2149.port.select_l(&cf2149, cf2149_clk_cycle(cycle), port.i_sel_l);

		const union cf2149_bdc bdc = {
			.bdir = port.i_bdir,
			.bc2  = port.i_bc2,
			.bc1  = port.i_bc1,
		};
		cf2149.port.bdc(&cf2149, cf2149_clk_cycle(cycle), bdc);

		if (bdc.u8 == CF2149_BDC_ADAR ||
		    bdc.u8 == CF2149_BDC_BAR  ||
		    bdc.u8 == CF2149_BDC_DWS  ||
		    bdc.u8 == CF2149_BDC_INTAK)
			cf2149.port.wr_da(&cf2149, cf2149_clk_cycle(cycle), port.i_da);
		else if (bdc.u8 == CF2149_BDC_DTB)
			cf2149.port.rd_da(&cf2149, cf2149_clk_cycle(cycle));

	}

	svs_entry(svs_file, cycle, &cf2149, ac[0]);

	free(line);

	fclose(svs_file);
	fclose(psb_file);

	if (error)
		goto err;
	else if (rename(tmp_path, svs_path) == -1)
		goto err;

	return EXIT_SUCCESS;

err:
	remove(tmp_path);

	return EXIT_FAILURE;
}
