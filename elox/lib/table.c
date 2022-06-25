#include <stdlib.h>
#include <string.h>

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"

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

static Entry *findEntry(Entry *entries, int capacity, ObjString *key) {
	uint32_t index = key->hash & (capacity - 1);
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

static int findEntryIndex(Entry *entries, int capacity, ObjString *key) {
	uint32_t index = key->hash & (capacity - 1);

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

	Entry *entry = findEntry(table->entries, table->capacity, key);
	if (entry->key == NULL)
		return false;

	*value = entry->value;
	return true;
}

int tableGetIndex(Table *table, ObjString *key) {
	if (table->count == 0)
		return -1;

	return findEntryIndex(table->entries, table->capacity, key);
}

static void adjustCapacity(VMCtx *vmCtx, Table *table, int capacity) {
	Entry *entries = ALLOCATE(vmCtx, Entry, capacity);
	for (int i = 0; i < capacity; i++) {
		entries[i].key = NULL;
		entries[i].value = NIL_VAL;
	}

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		if (entry->key == NULL)
			continue;

		Entry *dest = findEntry(entries, capacity, entry->key);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(vmCtx, Entry, table->entries, table->capacity);
	table->entries = entries;
	table->capacity = capacity;
}

bool tableSet(VMCtx *vmCtx, Table *table, ObjString *key, Value value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		adjustCapacity(vmCtx, table, capacity);
	}

	Entry *entry = findEntry(table->entries, table->capacity, key);
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

	Entry *entry = findEntry(table->entries, table->capacity, key);
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
	Entry *entry = findEntry(table->entries, table->capacity, key);
	if (entry->key == NULL)
		return false;

	// Place a tombstone in the entry.
	entry->key = NULL;
	entry->value = BOOL_VAL(true);
	return true;
}

void tableAddAll(VMCtx *vmCtx, Table *from, Table *to) {
	for (int i = 0; i < from->capacity; i++) {
		Entry *entry = &from->entries[i];
		if (entry->key != NULL)
			tableSet(vmCtx, to, entry->key, entry->value);
	}
}

ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash) {
	if (table->count == 0)
		return NULL;

	uint32_t index = hash & (table->capacity - 1);
	for (;;) {
		Entry *entry = &table->entries[index];
		if (entry->key == NULL) {
			// Stop if we find an empty non-tombstone entry.
			if (IS_NIL(entry->value))
				return NULL;
		} else if (entry->key->string.length == length &&
				   entry->key->hash == hash &&
				   memcmp(entry->key->string.chars, chars, length) == 0) {
			// We found it.
			return entry->key;
		}

		index = (index + 1) & (table->capacity - 1);
	}
}

void tableRemoveWhite(Table *table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry *entry = &table->entries[i];
		if (entry->key != NULL && !entry->key->obj.isMarked)
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
