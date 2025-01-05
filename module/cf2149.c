// SPDX-License-Identifier: GPL-2.0
/* Copyright (C) 2025 Fredrik Noring */

#include "cf2149/macro.h"

#include "cf2149/module/assert.h"
#include "cf2149/module/cf2149.h"

#define DEFINE_CF2149_CHN_PERIOD(ch_)					\
static uint16_t cf2149_ch ## ch_ ## _period(				\
	const struct cf2149_module *module)				\
{									\
	const uint16_t period =						\
		(module->state.reg.hi_ ## ch_.period << 8) |		\
		 module->state.reg.lo_ ## ch_.period;			\
									\
	return period ? period : 1;					\
}

DEFINE_CF2149_CHN_PERIOD(a)
DEFINE_CF2149_CHN_PERIOD(b)
DEFINE_CF2149_CHN_PERIOD(c)

#define DEFINE_CF2149_CHN_UPDATE(ch_)					\
static bool cf2149_ch ## ch_ ## _update(struct cf2149_module *module)	\
{									\
	const uint16_t period = cf2149_ch ## ch_ ## _period(module);	\
	const bool t = module->state.tone.ch_.p >= period;		\
									\
	if (++module->state.tone.ch_.p >= 2 * period)			\
		module->state.tone.ch_.p = 0;				\
									\
	return t;							\
}

DEFINE_CF2149_CHN_UPDATE(a)
DEFINE_CF2149_CHN_UPDATE(b)
DEFINE_CF2149_CHN_UPDATE(c)

#define DEFINE_CF2149_MXN(ch_)						\
static bool cf2149_mx ## ch_(const struct cf2149_module *module,	\
	const bool t, const bool n)					\
{									\
	return (module->state.reg.iomix.tone_  ## ch_ || t) &&		\
	       (module->state.reg.iomix.noise_ ## ch_ || n);		\
}

DEFINE_CF2149_MXN(a)
DEFINE_CF2149_MXN(b)
DEFINE_CF2149_MXN(c)

static uint16_t cf2149_noise_period(const struct cf2149_module *module)
{
	return module->state.reg.noise.period ?
	       module->state.reg.noise.period : 1;
}

static bool cf2149_rng_update(struct cf2149_module *module)
{
	struct cf2149_noise_generator *ng = &module->state.noise;
	const bool r = ng->lfsr & 1;

	if (ng->p >= 2 * cf2149_noise_period(module))
		ng->p = 0;
	if (!ng->p++)		/* 17 stage LFSR with a period of 131072. */
		ng->lfsr = (ng->lfsr >> 1)
			   | ((   !(ng->lfsr)
			      ^ (!!(ng->lfsr & 1))
			      ^ (!!(ng->lfsr & 8))) << 16);

	return r;
}

static uint16_t cf2149_env_period(const struct cf2149_module *module)
{
	const uint16_t period = (module->state.reg.envelope_hi.period << 8) |
				 module->state.reg.envelope_lo.period;

	return period ? period : 1;
}

static uint8_t cf2149_env_level(struct cf2149_module *module)
{
#define RISE  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, \
             16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
#define FALL 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, \
             15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0
#define ZERO  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
#define HOLD 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, \
             31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31

	static const uint8_t wave[16][3 * 32] = {
		{ FALL, ZERO, ZERO },	/* \___ */
		{ FALL, ZERO, ZERO },	/* \___ */
		{ FALL, ZERO, ZERO },	/* \___ */
		{ FALL, ZERO, ZERO },	/* \___ */
		{ RISE, ZERO, ZERO },	/* /___ */
		{ RISE, ZERO, ZERO },	/* /___ */
		{ RISE, ZERO, ZERO },	/* /___ */
		{ RISE, ZERO, ZERO },	/* /___ */
		{ FALL, FALL, FALL },	/* \\\\ */
		{ FALL, ZERO, ZERO },	/* \___ */
		{ FALL, RISE, FALL },	/* \/\/ */
		{ FALL, HOLD, HOLD },	/* \--- */
		{ RISE, RISE, RISE },	/* //// */
		{ RISE, HOLD, HOLD },	/* /--- */
		{ RISE, FALL, RISE },	/* /\/\ */
		{ RISE, ZERO, ZERO },	/* /___ */
	};
	const uint8_t w = wave[module->state.reg.envelope_shape.ctrl]
			      [module->state.env.wave];

	if (++module->state.env.p >= cf2149_env_period(module)) {
		module->state.env.p = 0;

		if (++module->state.env.wave >= 3 * 32)
			module->state.env.wave -= 2 * 32;
	}

	return (w << 3) | (w ? 0x7 : 0);  /* FIXME: Adaptable rounding mode. */
}

#define CF2149_LVN(ch_)							\
static uint8_t cf2149_lv ## ch_(const struct cf2149_module *module,	\
	const bool mx, const uint8_t lvl, const uint8_t env)		\
{									\
	return mx ? (module->state.reg.level_ ## ch_.m ? env : lvl) : 0;\
}

CF2149_LVN(a)
CF2149_LVN(b)
CF2149_LVN(c)

static uint8_t cf2149_level_ext(uint8_t lvl)
{
	/* FIXME: Adaptable rounding mode. */
	return (lvl << 4) | (lvl ? 0xf : 0);
}

static size_t cf2149_rd_ac(struct cf2149_module *module,
	struct cf2149_clk clk, struct cf2149_ac *buffer, size_t count)
{
	if (!module->port.state.reset_l)
		return 0;

	const uint8_t xlva = cf2149_level_ext(module->state.reg.level_a.level);
	const uint8_t xlvb = cf2149_level_ext(module->state.reg.level_b.level);
	const uint8_t xlvc = cf2149_level_ext(module->state.reg.level_c.level);

	const uint64_t cd = module->port.state.select_l ? 8 : 16;
	size_t i = 0;

	for (; module->clk.c < clk.c && i < count; module->clk.c += cd, i++) {
		const bool cha = cf2149_cha_update(module);
		const bool chb = cf2149_chb_update(module);
		const bool chc = cf2149_chc_update(module);
		const bool rng = cf2149_rng_update(module);

		const bool mxa = cf2149_mxa(module, cha, rng);
		const bool mxb = cf2149_mxb(module, chb, rng);
		const bool mxc = cf2149_mxc(module, chc, rng);

		const uint8_t env = cf2149_env_level(module);
		const uint8_t lva = cf2149_lva(module, mxa, xlva, env);
		const uint8_t lvb = cf2149_lvb(module, mxb, xlvb, env);
		const uint8_t lvc = cf2149_lvc(module, mxc, xlvc, env);

		buffer[i] = (struct cf2149_ac) {
			.lva.u8 = lva,
			.lvb.u8 = lvb,
			.lvc.u8 = lvc,
		};
	}

	return i;
}

static uint8_t cf2149_rd_da(struct cf2149_module *module, struct cf2149_clk clk)
{
	if (!module->port.state.reset_l || module->port.state.a98.u8 != 1)
		return 0x00;

	switch (module->port.state.bdc.u8) {
	case CF2149_BDC_DTB:
		return module->state.reg_address < 16 ?
		       module->state.reg.u8[module->state.reg_address] : 0xff;
	default:
		return 0xff;	/* Inactive */
	}
}

static void cf2149_wr_da(struct cf2149_module *module,
	struct cf2149_clk clk, uint8_t da)
{
	MODULE_BUG_ON(module, module->clk.c < clk.c);

	if (!module->port.state.reset_l || module->port.state.a98.u8 != 1)
		return;

	switch (module->port.state.bdc.u8) {
	case CF2149_BDC_ADAR:
	case CF2149_BDC_BAR:
	case CF2149_BDC_INTAK:
		if (da < 16)
			module->state.reg_address = da;
		break;
	case CF2149_BDC_DWS:
		if (module->state.reg_address == CF2149_REG_SHAPE &&
		    module->state.reg.u8[module->state.reg_address] != da)
			module->state.env.p = module->state.env.wave = 0;

		if (module->state.reg_address < 16)
			module->state.reg.u8[module->state.reg_address] = da;
		break;
	default:
		/* Inactive */
		break;
	}
}

static void cf2149_select_l(struct cf2149_module *module,
	struct cf2149_clk clk, enum cf2149_select_mode select_l)
{
	MODULE_BUG_ON(module, module->clk.c < clk.c);

	module->port.state.select_l = select_l;
}

static void cf2149_bdc(struct cf2149_module *module,
	struct cf2149_clk clk, union cf2149_bdc bdc)
{
	module->port.state.bdc = bdc;
}

static void cf2149_a98(struct cf2149_module *module,
	struct cf2149_clk clk, union cf2149_a98 a98)
{
	module->port.state.a98 = a98;
}

static void cf2149_reset_l(struct cf2149_module *module,
	struct cf2149_clk clk, bool reset_l)
{
	if (!reset_l)
		module->state = (struct cf2149_state) { };
	else if (reset_l && !module->port.state.reset_l)
		module->clk = clk;

	module->port.state.reset_l = reset_l;
}

struct cf2149_module cf2149_init(uint32_t mode)
{
	struct cf2149_module module = {
		.port = {
			.reset_l  = cf2149_reset_l,
			.select_l = cf2149_select_l,
			.bdc      = cf2149_bdc,
			.a98      = cf2149_a98,

			.rd_da    = cf2149_rd_da,
			.wr_da    = cf2149_wr_da,
			.rd_ac    = cf2149_rd_ac,

			.state = {
				/* RESET_L has a pull-up resistance. */
				.reset_l = 1,
				/*
				 * SELECT_L is specific to YM2149. It has a
				 * pull-up resistor for compatibility with
				 * AY-3-8910 where this is TEST_2.
				 */
				.select_l = CF2149_SELECT_MODE_CLKDIV8,
				/*
				 * A9_L and A8 may be left unconnected as each
				 * is provided with either an on-chip pull-down
				 * (A9_L) or pull-up (A8) resistor.
				 */
				.a98 = {
					.a9_l = 0,
					.a8   = 1,
				},
			},
		},
	};

	BUILD_BUG_ON(sizeof(module.state.reg) != 16);

	return module;
};
