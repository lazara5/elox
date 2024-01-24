// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"
#include "elox/vm.h"

#include <stdlib.h>
#include <string.h>

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freeTable(VMCtx *vmCtx, Table *table) {
	FREE_ARRAY(vmCtx, Entry, table->entries, table->capacity);
	initTable(table);
}

// Fibonacci hashing, see
// https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
static inline uint32_t indexFor(uint32_t hash, uint32_t shift) {
	return (hash * 2654435769u) >> shift;
}

static Entry *findEntry(Entry *entries, int capacity, uint32_t shift, ObjString *key) {
	//uint32_t index = key->hash & (capacity - 1);
	uint32_t index =  indexFor(key->hash, shift);
	Entry *tombstone = NULL;

	for (;;) {
		Entry *entry = &entries[index];
		if (entry->key == NULL) {
			if (IS_NIL(entry->value)) {
				// Empty entry.
				return tombstone != NULL ? tombstone : entry;
			} else {
				// We found a tombstone.
				if (tombstone == NULL)
					tombstone = entry;
			}
		} else if (entry->key == key) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

static int findEntryIndex(Entry *entries, int capacity, uint32_t shift, ObjString *key) {
	//uint32_t index = key->hash & (capacity - 1);
	uint32_t index = indexFor(key->hash, shift);

	for (;;) {
		Entry *entry = &entries[index];
		if (entry->key == NULL) {
			if (IS_NIL(entry->value)) {
				// Empty entry
				return -1;
			}
		} else if (entry->key == key) {
			// We found the key
			return index;
		}

		index = (index + 1) & (capacity - 1);
	}
}

bool tableGet(Table *table, ObjString *key, Value *value) {
	if (table->count == 0)
		return false;

	Entry *entry = findEntry(table->entries, table->capacity, table->shift, key);
	if (entry->key == NULL)
		return false;

	*value = entry->value;
	return true;
}

int tableGetIndex(Table *table, ObjString *key) {
	if (table->count == 0)
		return -1;

	return findEntryIndex(table->entries, table->capacity, table->shift, key);
}

static bool adjustCapacity(VMCtx *vmCtx, Table *table, uint32_t newCapacity) {
	Entry *newEntries = ALLOCATE(vmCtx, Entry, newCapacity);
	if (ELOX_UNLIKELY(newEntries == NULL))
		return false;
	for (uint32_t i = 0; i < newCapacity; i++) {
		newEntries[i].key = NULL;
		newEntries[i].value = NIL_VAL;
	}

	uint32_t log2Size = ELOX_CTZ(newCapacity);
	uint32_t shift = 8 * sizeof(uint32_t) - log2Size;

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		if (entry->key == NULL)
			continue;

		Entry *dest = findEntry(newEntries, newCapacity, shift, entry->key);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(vmCtx, Entry, table->entries, table->capacity);
	table->entries = newEntries;
	table->capacity = newCapacity;
	table->shift = shift;

	return true;
}

bool tableSet(Table *table, ObjString *key, Value value, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		bool adjusted = adjustCapacity(vmCtx, table, capacity);
		ELOX_COND_RAISE_RET_VAL((!adjusted), error, OOM(), false);
	}

	Entry *entry = findEntry(table->entries, table->capacity, table->shift, key);
	bool isNewKey = (entry->key == NULL);
	if (isNewKey && IS_NIL(entry->value))
		table->count++;

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

Value tableSetIfMissing(VMCtx *vmCtx, Table *table, ObjString *key, Value value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		adjustCapacity(vmCtx, table, capacity);
	}

	Entry *entry = findEntry(table->entries, table->capacity, table->shift, key);
	bool isNewKey = (entry->key == NULL);
	if (isNewKey && IS_NIL(entry->value))
		table->count++;
	else
		return entry->value;

	entry->key = key;
	entry->value = value;
	return value;
}

bool tableDelete(Table *table, ObjString *key) {
	if (table->count == 0)
		return false;

	// Find the entry.
	Entry *entry = findEntry(table->entries, table->capacity, table->shift, key);
	if (entry->key == NULL)
		return false;

	// Place a tombstone in the entry.
	entry->key = NULL;
	entry->value = BOOL_VAL(true);
	return true;
}

void tableAddAll(Table *from, Table *to, Error *error) {
	for (int i = 0; i < from->capacity; i++) {
		Entry *entry = &from->entries[i];
		if (entry->key != NULL) {
			tableSet(to, entry->key, entry->value, error);
			if (ELOX_UNLIKELY(error->raised))
				return;
		}
	}
}

ObjString *tableFindString(Table *table, const uint8_t *chars, int length, uint32_t hash) {
	if (table->count == 0)
		return NULL;

	//uint32_t index = hash & (table->capacity - 1);
	uint32_t index = indexFor(hash, table->shift);
	for (;;) {
		Entry *entry = &table->entries[index];
		if (entry->key == NULL) {
			// Stop if we find an empty non-tombstone entry
			if (IS_NIL(entry->value))
				return NULL;
		} else if (entry->key->string.length == length &&
				   entry->key->hash == hash &&
				   memcmp(entry->key->string.chars, chars, length) == 0) {
			// We found it
			return entry->key;
		}

		index = (index + 1) & (table->capacity - 1);
	}
}

bool tableGetString(Table *table, const uint8_t *chars, int length, uint32_t hash, Value *value) {
	if (table->count == 0)
		return false;

	uint32_t index = indexFor(hash, table->shift);
	for (;;) {
		Entry *entry = &table->entries[index];
		if (entry->key == NULL) {
			// Stop if we find an empty non-tombstone entry
			if (IS_NIL(entry->value))
				return false;
		} else if (entry->key->string.length == length &&
				   entry->key->hash == hash &&
				   memcmp(entry->key->string.chars, chars, length) == 0) {
			// We found it
			*value = entry->value;
			return true;
		}

		index = (index + 1) & (table->capacity - 1);
	}
}

void tableRemoveWhite(Table *table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		if (entry->key != NULL && (entry->key->obj.markers == 0))
			tableDelete(table, entry->key);
	}
}

void markTable(VMCtx *vmCtx, Table *table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		markObject(vmCtx, (Obj *)entry->key);
		markValue(vmCtx, entry->value);
	}
}
