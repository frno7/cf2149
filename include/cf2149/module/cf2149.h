// SPDX-License-Identifier: GPL-2.0
/* Copyright (C) 2025 Fredrik Noring */

#ifndef CF2149_MODULE_CF2149_H
#define CF2149_MODULE_CF2149_H

#include "cf2149/types.h"

#define CF2149_REGISTERS(reg)						\
	reg( 0, plo_a,   PLO_A,   "Period of channel A fine tone")	\
	reg( 1, phi_a,   PHI_A,   "Period of channel A rough tone")	\
	reg( 2, plo_b,   PLO_B,   "Period of channel B fine tone")	\
	reg( 3, phi_b,   PHI_B,   "Period of channel B rough tone")	\
	reg( 4, plo_c,   PLO_C,   "Period of channel C fine tone")	\
	reg( 5, phi_c,   PHI_C,   "Period of channel C rough tone")	\
	reg( 6, noise,   NOISE,   "Period of noise")			\
	reg( 7, iomix,   IOMIX,   "I/O port and mixer settings")	\
	reg( 8, level_a, LEVEL_A, "Level of channel A")			\
	reg( 9, level_b, LEVEL_B, "Level of channel B")			\
	reg(10, level_c, LEVEL_C, "Level of channel C")			\
	reg(11, plo_env, PLO_ENV, "Period of envelope fine")		\
	reg(12, phi_env, PHI_ENV, "Period of envelope rough")		\
	reg(13, shape,   SHAPE,   "Shape of envelope")			\
	reg(14, io_a,    IO_A,    "Data of I/O port A")			\
	reg(15, io_b,    IO_B,    "Data of I/O port B")

enum {
#define CF2149_REG_ENUM(register_, symbol_, label_, description_)		\
	CF2149_REG_##label_ = register_,
CF2149_REGISTERS(CF2149_REG_ENUM)
};

struct cf2149_channel_lo {
	uint8_t period;
};

struct cf2149_channel_hi {
	__BITFIELD_FIELD(uint8_t : 4,
	__BITFIELD_FIELD(uint8_t period : 4,
	;))
};

struct cf2149_noise {
	__BITFIELD_FIELD(uint8_t : 3,
	__BITFIELD_FIELD(uint8_t period : 5,
	;))
};

struct cf2149_iomix {
	__BITFIELD_FIELD(uint8_t io_b : 1,
	__BITFIELD_FIELD(uint8_t io_a : 1,
	__BITFIELD_FIELD(uint8_t noise_c : 1,
	__BITFIELD_FIELD(uint8_t noise_b : 1,
	__BITFIELD_FIELD(uint8_t noise_a : 1,
	__BITFIELD_FIELD(uint8_t tone_c : 1,
	__BITFIELD_FIELD(uint8_t tone_b : 1,
	__BITFIELD_FIELD(uint8_t tone_a : 1,
	;))))))))
};

struct cf2149_level {
	__BITFIELD_FIELD(uint8_t : 3,
	__BITFIELD_FIELD(uint8_t m : 1,
	__BITFIELD_FIELD(uint8_t level : 4,
	;)))
};

struct cf2149_envelope_lo {
	uint8_t period;
};

struct cf2149_envelope_hi {
	uint8_t period;
};

struct cf2149_envelope_shape {
	union {
		__BITFIELD_FIELD(uint8_t : 4,
		__BITFIELD_FIELD(uint8_t cont : 1,
		__BITFIELD_FIELD(uint8_t att : 1,
		__BITFIELD_FIELD(uint8_t alt : 1,
		__BITFIELD_FIELD(uint8_t hold: 1,
		;)))))
		__BITFIELD_FIELD(uint8_t : 4,
		__BITFIELD_FIELD(uint8_t ctrl : 4,
		;))
	};
};

struct cf2149_io_a {
	uint8_t data;
};

struct cf2149_io_b {
	uint8_t data;
};

union cf2149_reg {
	struct {
		struct cf2149_channel_lo lo_a;
		struct cf2149_channel_hi hi_a;
		struct cf2149_channel_lo lo_b;
		struct cf2149_channel_hi hi_b;
		struct cf2149_channel_lo lo_c;
		struct cf2149_channel_hi hi_c;
		struct cf2149_noise noise;
		struct cf2149_iomix iomix;
		struct cf2149_level level_a;
		struct cf2149_level level_b;
		struct cf2149_level level_c;
		struct cf2149_envelope_lo envelope_lo;
		struct cf2149_envelope_hi envelope_hi;
		struct cf2149_envelope_shape envelope_shape;
		struct cf2149_io_a io_a;
		struct cf2149_io_b io_b;
	};
	uint8_t r[16];
};

struct cf2149_ac {
	uint8_t lva;
	uint8_t lvb;
	uint8_t lvc;
};

struct cf2149_clk {
	uint64_t c;
};

/* Bus direction, bus control 2, 1 */
enum {				/* BDIR BC2 BC1 Mode          */
	CF2149_BDC_NACT,	/*   0   0   0  Inactive      */
	CF2149_BDC_ADAR,	/*   0   0   1  Latch address */
	CF2149_BDC_IAB,		/*   0   1   0  Inactive      */
	CF2149_BDC_DTB,		/*   0   1   1  Read from PSG */
	CF2149_BDC_BAR,		/*   1   0   0  Latch address */
	CF2149_BDC_DW,		/*   1   0   1  Inactive      */
	CF2149_BDC_DWS,		/*   1   1   0  Write to PSG  */
	CF2149_BDC_INTAK,	/*   1   1   1  Latch address */
};

union cf2149_bdc {
	struct {
		__BITFIELD_FIELD(uint8_t : 5,
		__BITFIELD_FIELD(uint8_t bdir : 1,
		__BITFIELD_FIELD(uint8_t bc2 : 1,
		__BITFIELD_FIELD(uint8_t bc1 : 1,
		;))))
	};
	uint8_t r;
};

union cf2149_a98 {
	struct {
		__BITFIELD_FIELD(uint8_t : 6,
		__BITFIELD_FIELD(uint8_t a9_l : 1,
		__BITFIELD_FIELD(uint8_t a8 : 1,
		;)))
	};
	uint8_t r;
};

enum cf2149_select_mode {
	CF2149_SELECT_MODE_CLKDIV16,
	CF2149_SELECT_MODE_CLKDIV8,
};

struct cf2149_module;

struct cf2149_port {
	void (*reset_l)(struct cf2149_module *module,
		struct cf2149_clk clk, bool reset_l);
	void (*select_l)(struct cf2149_module *module,
		struct cf2149_clk clk, enum cf2149_select_mode mode);
	void (*bdc)(struct cf2149_module *module,
		struct cf2149_clk clk, union cf2149_bdc bdc);
	void (*a98)(struct cf2149_module *module,
		struct cf2149_clk clk, union cf2149_a98 a98);

	uint8_t (*rd_da)(struct cf2149_module *module, struct cf2149_clk clk);
	void (*wr_da)(struct cf2149_module *module,
		struct cf2149_clk clk, uint8_t da);
	size_t (*rd_ac)(struct cf2149_module *module,
		struct cf2149_clk clk, struct cf2149_ac *buffer, size_t count);

	struct cf2149_port_state {
		bool reset_l;
		enum cf2149_select_mode select_l;
		union cf2149_bdc bdc;
		union cf2149_a98 a98;
	} state;
};

struct cf2149_noise_generator {
	uint32_t p;
	uint32_t lfsr;
};

struct cf2149_module {
	struct cf2149_port port;

	struct cf2149_clk clk;

	struct cf2149_state {
		union cf2149_reg reg;
		uint8_t reg_address;

		struct {
			struct {
				uint16_t p;
			} a, b, c;
		} tone;

		struct {
			uint32_t p;
			uint32_t wave;
		} env;

		struct cf2149_noise_generator noise;
	} state;

	struct {
		void (*report)(const char *fmt, ...)
			__attribute__((format(printf, 1, 2)));
	} debug;
};

struct cf2149_module cf2149_init();

static inline struct cf2149_clk cf2149_clk_cycle(uint64_t c)
{
	return (struct cf2149_clk) { .c = c };
}

#endif /* CF2149_MODULE_CF2149_H */
