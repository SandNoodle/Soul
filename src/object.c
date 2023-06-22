#include "object.h"

#include "soul.h"

// HASH: 64-bit FNV-1a
static uint64_t soul__string_hash(char* str, size_t length)
{
	uint64_t hash = 0xcbf29ce484222325;

	for(size_t i = 0; i < length; ++i)
	{
		hash ^= str[i];
		hash *= 0x00000100000001B3;
	}

	return hash;
}

soul_string_obj_t* soul__string_obj_new(char* str, size_t length)
{
	soul_string_obj_t* string = (soul_string_obj_t*)malloc(sizeof(soul_string_obj_t));
	string->data = str;
	string->length = length;
	string->hash = soul__string_hash(str, length);
	return string;
}

void soul__string_obj_free(soul_string_obj_t* string)
{
	free(string->data);
	string->data = NULL;
	string->hash = 0;
	string->length = 0;
}

bool soul__string_obj_equals(soul_string_obj_t* a, soul_string_obj_t* b)
{
	return (a->length == b->length)
		&& memcmp(a->data, b->data, a->length) == 0;
}
