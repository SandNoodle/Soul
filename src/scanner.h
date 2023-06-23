#pragma once

#include "soul.h"
#include "token.h"

typedef struct {
	const char* start;
	const char* current;
	uint32_t line;
} soul_scanner_t;

struct soul_scanner_config_t {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
};
