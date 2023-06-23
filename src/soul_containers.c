#include "soul_containers.h"

#include <stdio.h>
#include "soul_config.h"
#include "object.h"
#include "value.h"

//
// Hashtable
//

#define SOUL_HASHTABLE_TOMBSTONE \
	((soul_value_t){ .type = SOUL_TYPE_BOOL, .as.type_bool = true })

#define SOUL_HASHTABLE_IS_TOMBSTONE(x) \
	((x).type == SOUL_TYPE_BOOL && (x).as.type_bool == true)

struct soul_hashtable_entry_t {
	soul_string_obj_t* key;
	soul_value_t value;
};

struct soul_hashtable_t {
	soul_hashtable_entry_t* entries;
	size_t size;
	size_t capacity;
};

static soul_hashtable_entry_t* soul__hashtable__find_entry(soul_hashtable_t*, soul_string_obj_t*);
static void soul__hashtable__grow_capacity(soul_hashtable_t*, size_t);

void soul__hashtable_new(soul_hashtable_t* h)
{
	h->capacity = SOUL_GROW_CAPACITY(0);
	h->entries = (soul_hashtable_entry_t*)malloc(sizeof(soul_hashtable_entry_t) * h->capacity);
	h->size = 0;
	for(size_t index = 0; index < h->capacity; ++index)
	{
		h->entries[index].key = NULL;
		h->entries[index].value =
			(soul_value_t) { SOUL_TYPE_NULL, .as.type_bool = false };
	}
}

void soul__hashtable_free(soul_hashtable_t* h)
{
	free(h->entries);
	h->entries = NULL;
	h->size = 0;
	h->capacity = 0;
}

bool soul__hashtable_insert(soul_hashtable_t* h, soul_string_obj_t* key, soul_value_t value)
{
	if(h->size + 1 > h->capacity * SOUL_HASHTABLE_MAX_LOAD)
	{
		size_t capacity = SOUL_GROW_CAPACITY(h->capacity);
		soul__hashtable__grow_capacity(h, capacity);
	}

	soul_hashtable_entry_t* entry = soul__hashtable__find_entry(h, key);
	bool is_new_entry = !entry->key;
	bool is_value_null = entry->value.type == SOUL_TYPE_NULL;
	if(is_new_entry && is_value_null)
		h->size++;

	*entry = (soul_hashtable_entry_t){ key, value };
	return is_new_entry;
}

bool soul__hashtable_remove(soul_hashtable_t* h, soul_string_obj_t* key)
{
	if(h->size == 0) return false;

	soul_hashtable_entry_t* entry = soul__hashtable__find_entry(h, key);
	if(entry->key == NULL) return false;

	// Place tombstone
	entry->key = NULL;
	entry->value = SOUL_HASHTABLE_TOMBSTONE;

	return true;
}

soul_hashtable_entry_t* soul__hashtable_find(soul_hashtable_t* h, soul_string_obj_t* key)
{
	if(!h->entries) return NULL;
	if(!key) return NULL;
	return soul__hashtable__find_entry(h, key);
}

static soul_hashtable_entry_t* soul__hashtable__find_entry(soul_hashtable_t* h, soul_string_obj_t* key)
{
	size_t index = key->hash % h->capacity;
	soul_hashtable_entry_t* tombstone = NULL;
	for(;;)
	{
		soul_hashtable_entry_t* entry = &h->entries[index];
		if(!entry->key)
		{
			if(entry->value.type == SOUL_TYPE_NULL)
			{
				return tombstone ? tombstone : entry; // Empty entry.
			}
			else if(!tombstone)
			{
				tombstone = entry;
			}
		} else if (soul__string_obj_equals(entry->key, key))
		{
			return entry;
		}

		index = (index + 1) % h->capacity;
	}

	return NULL;
}

static void soul__hashtable__grow_capacity(soul_hashtable_t* h, size_t capacity)
{
	soul_hashtable_entry_t* new_entries = (soul_hashtable_entry_t*)malloc(sizeof(soul_hashtable_entry_t) * capacity);
	for(size_t index = 0; index < h->capacity; ++index)
	{
		new_entries[index].key = NULL;
		new_entries[index].value = (soul_value_t){ SOUL_TYPE_NULL, .as.type_bool = false };
	}

	h->size = 0;
	for(size_t index = 0; index < h->capacity; ++index)
	{
		soul_hashtable_entry_t* entry = &h->entries[index];
		if(entry->key == NULL)
			continue;

		soul_hashtable_entry_t* dest = soul__hashtable__find_entry(h, entry->key);
		dest->key = entry->key;
		dest->value = entry->value;
		h->size++;
	}

	free(h->entries);
	h->entries = new_entries;
	h->capacity = capacity;
}

#undef SOUL_HASHTABLE_TOMBSTONE
#undef SOUL_HASHTABLE_IS_TOMBSTONE

