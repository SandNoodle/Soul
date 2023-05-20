#ifndef SOUL_SOUL_CONTAINERS_H
#define SOUL_SOUL_CONTAINERS_H

// @TODO Replace malloc/realloc/free with user provided allocator.

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
		soul_valid_t valid;                                              \
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

#endif // SOUL_SOUL_CONTAINERS_H
