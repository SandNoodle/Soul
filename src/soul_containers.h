#ifndef SOUL_SOUL_CONTAINERS_H
#define SOUL_SOUL_CONTAINERS_H

#include "soul_fwd.h"

// @TODO Replace malloc/realloc/free with user provided allocator.
// @TODO Current implementation is heavily using preprocessor macros,
//       which throws off any debbuging! Also it makes writing new code APINTA :C
//       I'd prefer to reimplement it with 'void*' in the future.

//
// Vector
//

#define SOUL_GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)
#define SOUL_GROW_ARRAY(type, ptr, capacity) (type*)realloc((ptr), (capacity) * sizeof(type))

#define SOUL_VECTOR_DEFINE(name, type)                                   \
	typedef struct {                                                     \
		type* data;                                                      \
		size_t size;                                                     \
		size_t capacity;                                                 \
		bool valid;                                                      \
	} soul_##name##_vector_t;                                            \
                                                                         \
	void soul__##name##_vector_new(soul_##name##_vector_t* vector);      \
	void soul__##name##_vector_free(soul_##name##_vector_t* vector);     \
	void soul__##name##_vector_push(soul_##name##_vector_t* v, type t);

#define SOUL_VECTOR_DECLARE(name, type)                                  \
	void soul__##name##_vector_new(soul_##name##_vector_t* v)            \
	{                                                                    \
		v->size = 0;                                                     \
		v->capacity = SOUL_GROW_CAPACITY(0);                             \
		v->data = SOUL_GROW_ARRAY(type, NULL, v->capacity);              \
		v->valid = true;                                                 \
	}                                                                    \
                                                                         \
	void soul__##name##_vector_free(soul_##name##_vector_t* v)           \
	{                                                                    \
		free(v->data);                                                   \
		v->data = NULL;                                                  \
		v->size = 0;                                                     \
		v->capacity = 0;                                                 \
		v->valid = false;                                                \
	}                                                                    \
                                                                         \
	void soul__##name##_vector_push(soul_##name##_vector_t* v, type t)   \
	{                                                                    \
		if(v->size + 1 > v->capacity)                                    \
		{                                                                \
			v->capacity = SOUL_GROW_CAPACITY(v->capacity);               \
			v->data = SOUL_GROW_ARRAY(type, v->data, v->capacity);       \
		}                                                                \
		v->data[v->size++] = t;                                          \
	}

//
// Stack
//

#define SOUL_STACK_DEFINE(name, type)                                     \
	typedef struct {                                                      \
		type* data;                                                       \
		size_t size;                                                      \
		size_t capacity;                                                  \
	} soul_##name##_stack_t;                                              \
                                                                          \
	void soul__##name##_stack_new(soul_##name##_stack_t* stack);          \
	void soul__##name##_stack_free(soul_##name##_stack_t* stack);         \
	void soul__##name##_stack_push(soul_##name##_stack_t* stack, type t); \
	type soul__##name##_stack_pop(soul_##name##_stack_t* stack);          \
	type soul__##name##_stack_peek(soul_##name##_stack_t* stack);

#define SOUL_STACK_DECLARE(name, type)                                    \
	void soul__##name##_stack_new(soul_##name##_stack_t* s)               \
	{                                                                     \
		s->size = 0;                                                      \
		s->capacity = SOUL_GROW_CAPACITY(0);                              \
		s->data = SOUL_GROW_ARRAY(type, NULL, s->capacity);               \
	}                                                                     \
                                                                          \
	void soul__##name##_stack_free(soul_##name##_stack_t* s)              \
	{                                                                     \
		free(s->data);                                                    \
		s->data = NULL;                                                   \
		s->size = 0;                                                      \
		s->capacity = 0;                                                  \
	}                                                                     \
                                                                          \
	void soul__##name##_stack_push(soul_##name##_stack_t* s, type t)      \
	{                                                                     \
		if(s->size + 1 > s->capacity)                                     \
		{                                                                 \
			s->capacity = SOUL_GROW_CAPACITY(s->capacity);                \
			s->data = SOUL_GROW_ARRAY(type, s->data, s->capacity);        \
		}                                                                 \
		s->data[s->size++] = t;                                           \
	}                                                                     \
                                                                          \
	type soul__##name##_stack_pop(soul_##name##_stack_t* s)               \
	{                                                                     \
		if(s->size < 1)                                                   \
		{                                                                 \
			/* @TODO Error!*/                                             \
		}                                                                 \
                                                                          \
		type t = s->data[s->size - 1];                                    \
		s->size--;                                                        \
		return t;                                                         \
	}                                                                     \
                                                                          \
	type soul__##name##_stack_peek(soul_##name##_stack_t* s)              \
	{                                                                     \
		return s->data[s->size];                                          \
	}

//
// Hash Table
//

// Initializes the new hashtable.
void soul__hashtable_new(soul_hashtable_t*);

// Frees given hashtable.
void soul__hashtable_free(soul_hashtable_t*);

// Inserts new entry into hashtable or updates value of existing one.
bool soul__hashtable_insert(soul_hashtable_t*,
	soul_string_obj_t* key, soul_value_t value);

// Removes given entry from hashtable. Returns true if entry was removed.
bool soul__hashtable_remove(soul_hashtable_t*,
	soul_string_obj_t* key);

// Returns entry from hashtable. If entry was not found it returns NULL.
soul_hashtable_entry_t* soul__hashtable_find(soul_hashtable_t*,
	soul_string_obj_t* key);

#endif // SOUL_SOUL_CONTAINERS_H
