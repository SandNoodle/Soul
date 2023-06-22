#ifndef SOUL_VALUE_H
#define SOUL_VALUE_H

#include "soul.h"

typedef enum {
	SOUL_TYPE_BOOL,
	SOUL_TYPE_INT,
	SOUL_TYPE_REAL,
	SOUL_TYPE_NULL,
} soul_value_type_t;

struct soul_value_t {
	soul_value_type_t type;
	union {
		uint8_t  type_bool; // Booleans
		int64_t  type_int;  // 64-bit (Signed) Integers
		double   type_real; // 64-bit Floating-Point Numbers
	} as;
};

#endif // SOUL_VALUE_H
