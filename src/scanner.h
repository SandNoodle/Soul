#pragma once

#include "soul.h"
#include "token.h"

typedef struct {
	const char* start;
	const char* current;
	uint32_t line;
} soul_scanner_t;
