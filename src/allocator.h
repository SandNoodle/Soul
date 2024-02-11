#ifndef SOUL_ALLOCATOR_H
#define SOUL_ALLOCATOR_H

#include <stdint.h>

typedef struct soul_allocator_t soul_allocator_t;
struct soul_allocator_t
{
	void* (*alloc)(size_t size, void* user_data);
	void* (*realloc)(void* ptr, size_t size, void* user_data);
	void (*free)(void* ptr, size_t size, void* user_data);
	void* user_data;
};

#endif //SOUL_ALLOCATOR_H
