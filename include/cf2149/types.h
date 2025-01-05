// SPDX-License-Identifier: GPL-2.0

#ifndef CF2149_TYPES_H
#define CF2149_TYPES_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define CF2149_BITFIELD(field, more) field; more
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define CF2149_BITFIELD(field, more) more field;
#else
#error "Bitfield neither big nor little endian?"
#endif

#endif /* CF2149_TYPES_H */
