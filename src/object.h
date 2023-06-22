#ifndef SOUL_OBJECT_H
#define SOUL_OBJECT_H

#include "soul.h"

struct soul_string_obj_t {
	char* data;
	size_t length;
	uint64_t hash;
};

soul_string_obj_t* soul__string_obj_new(char* str, size_t length);
void soul__string_obj_free(soul_string_obj_t*);
bool soul__string_obj_equals(soul_string_obj_t*, soul_string_obj_t*);

#endif // SOUL_OBJECT_H
