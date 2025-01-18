// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"
#include "elox/vm.h"
#include "elox/state.h"

#include <stdlib.h>
#include <string.h>

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freeTable(RunCtx *runCtx, Table *table) {
	FREE_ARRAY(runCtx, Entry, table->entries, table->capacity);
	initTable(table);
}

static int findEntryIndex(Entry *entries, int capacity, uint32_t shift, ObjString *key) {
	//uint32_t index = key->hash & (capacity - 1);
	uint32_t index = indexFor(key->hash, shift);

	for (;;) {
		Entry *entry = &entries[index];
		if (entry->key == NULL)
			return -1;
		else if (entry->key == key) {
			// We found the key
			return index;
		}

		index = (index + 1) & (capacity - 1);
	}
}

int tableGetIndex(Table *table, ObjString *key) {
	if (table->count == 0)
		return -1;

	return findEntryIndex(table->entries, table->capacity, table->shift, key);
}

static bool adjustCapacity(RunCtx *runCtx, Table *table, uint32_t newCapacity) {
	Entry *newEntries = ALLOCATE(runCtx, Entry, newCapacity);
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

	FREE_ARRAY(runCtx, Entry, table->entries, table->capacity);
	table->entries = newEntries;
	table->capacity = newCapacity;
	table->shift = shift;

	return true;
}

bool tableSet(RunCtx *runCtx, Table *table, ObjString *key, Value value, EloxError *error) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		bool adjusted = adjustCapacity(runCtx, table, capacity);
		ELOX_CHECK_THROW_RET_VAL(adjusted, error, OOM(runCtx), false);
	}

	Entry *entry = findEntry(table->entries, table->capacity, table->shift, key);
	bool isNewKey = (entry->key == NULL);
	if (isNewKey && IS_NIL(entry->value))
		table->count++;

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

Value tableSetIfMissing(RunCtx *runCtx, Table *table, ObjString *key, Value value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		adjustCapacity(runCtx, table, capacity);
		// TODO: check
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

#define NEXT_ENTRY(base, cap, entry) ((entry) + 1 != (base) + (cap) ? (entry) + 1 : (base))
#define MODULO_DISTANCE(cap, a, b) ((b) >= (a) ? (b) - (a) : (cap) + (b) - (a))

static ELOX_FORCE_INLINE void removeEntry(Table *table, Entry *toDelete) {
	Entry *base = table->entries;
	int cap = table->capacity;
	for (Entry *next = NEXT_ENTRY(base, cap, toDelete); ; next = NEXT_ENTRY(base, cap, next)) {
		if (next->key == NULL) {
			// next entry is empty
			toDelete->key = NULL;
			toDelete->value = NIL_VAL;
			table->count--;
			return;
		}

		Entry *natural = base + indexFor(next->key->hash, table->shift);
		if (MODULO_DISTANCE(cap, natural, toDelete) < MODULO_DISTANCE(cap, natural, next)) {
			// swap with next, then remove next
			*toDelete = *next;
			toDelete = next;
		 }
	}
}

bool tableDelete(Table *table, ObjString *key) {
	int index = findEntryIndex(table->entries, table->capacity, table->shift, key);
	if (index < 0)
		return false;

	removeEntry(table, table->entries + index);
	return true;
}

void tableAddAll(RunCtx *runCtx, Table *from, Table *to, EloxError *error) {
	for (int i = 0; i < from->capacity; i++) {
		Entry *entry = &from->entries[i];
		if (entry->key != NULL) {
			tableSet(runCtx, to, entry->key, entry->value, error);
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
		if (entry->key == NULL)
			return NULL;
		else if (entry->key->string.length == length &&
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
		if (entry->key == NULL)
			return false;
		else if (entry->key->string.length == length &&
				   entry->key->hash == hash &&
				   memcmp(entry->key->string.chars, chars, length) == 0) {
			// We found it
			*value = entry->value;
			return true;
		}

		index = (index + 1) & (table->capacity - 1);
	}
}

typedef struct {
	Entry *ptr;
} TableIter;

static void tableIterInit(TableIter *iter, const Table *table) {
	iter->ptr = table->entries;
}

bool tableIterNext(TableIter *iter, Table *table, Entry **crtEntry) {
		Entry *entry = iter->ptr;

		while (entry != &table->entries[table->capacity] && entry->key == NULL) {
			entry++;
		}

		if (entry != &table->entries[table->capacity]) {
			*crtEntry = entry;
			iter->ptr = entry + 1;
			return true;
		} else {
			iter->ptr = entry;
			return false;
		}
}

void tableIterRemove(TableIter *iter, Table *table) {
		Entry *nextEntry = iter->ptr;
		Entry *lastEntry = nextEntry - 1;

		removeEntry(table, lastEntry);

		iter->ptr = lastEntry;
}


void tableRemoveWhite(Table *table) {
	/*for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		if (entry->key != NULL && (entry->key->obj.markers == 0))
			tableDelete(table, entry->key);
	}*/
	TableIter iter;
	Entry *entry;

	tableIterInit(&iter, table);
	while (tableIterNext(&iter, table, &entry)) {
		if (entry->key->obj.markers == 0)
			tableIterRemove(&iter, table);
	}
}

void markTable(RunCtx *runCtx, Table *table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		markObject(runCtx, (Obj *)entry->key);
		markValue(runCtx, entry->value);
	}
}
