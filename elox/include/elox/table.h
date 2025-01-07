// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_TABLE_H
#define ELOX_TABLE_H

#include <elox/value.h>
#include <elox/object.h>

typedef struct {
	ObjString *key;
	Value value;
} Entry;

typedef struct {
	int count;
	int capacity;
	uint32_t shift;
	Entry *entries;
} Table;

void initTable(Table *table);
void freeTable(RunCtx *runCtx, Table *table);

// Fibonacci hashing, see
// https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
static inline uint32_t indexFor(uint32_t hash, uint32_t shift) {
	return (hash * 2654435769u) >> shift;
}

static inline Entry *findEntry(Entry *entries, int capacity, uint32_t shift, ObjString *key) {
	//uint32_t index = key->hash & (capacity - 1);
	uint32_t index =  indexFor(key->hash, shift);

	for (;;) {
		Entry *entry = &entries[index];
		if (entry->key == NULL) {
			return entry;
		} else if (entry->key == key) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

static inline bool tableGet(Table *table, ObjString *key, Value *value) {
	if (table->count == 0)
		return false;

	Entry *entry = findEntry(table->entries, table->capacity, table->shift, key);
	if (entry->key == NULL)
		return false;

	*value = entry->value;
	return true;
};

int tableGetIndex(Table *table, ObjString *key);
bool tableSet(Table *table, ObjString *key, Value value, Error *error);
Value tableSetIfMissing(RunCtx *runCtx, Table *table, ObjString *key, Value value);
bool tableDelete(Table *table, ObjString *key);
void tableAddAll(Table *from, Table *to, Error *error);
ObjString *tableFindString(Table *table, const uint8_t *chars, int length, uint32_t hash);
bool tableGetString(Table *table, const uint8_t *chars, int length, uint32_t hash, Value *value);
void tableRemoveWhite(Table *table);
void markTable(RunCtx *runCtx, Table *table);

#endif // ELOX_TABLE_H
