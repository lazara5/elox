// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_STRING_INT_TABLE_H
#define ELOX_STRING_INT_TABLE_H

#include <elox/value.h>
#include <elox/object.h>

typedef struct {
	ObjString *key;
	int32_t value;
} StringIntEntry;

typedef struct StringIntTable {
	int count;
	int capacity;
	uint32_t shift;
	StringIntEntry *entries;
} StringIntTable;

void initStringIntTable(StringIntTable *table);
void freeStringIntTable(RunCtx *runCtx, StringIntTable *table);

static inline StringIntEntry *stringIntTableFindEntry(StringIntEntry *entries, int capacity,
													  uint32_t shift, ObjString *key) {
	uint32_t index = tableIndexFor(key->hash, shift);

	for (;;) {
		StringIntEntry *entry = &entries[index];
		if (entry->key == NULL) {
			return entry;
		} else if (entry->key == key) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

static inline bool stringIntTableGet(StringIntTable *table, ObjString *key, int32_t *value) {
	if (table->count == 0)
		return false;

	StringIntEntry *entry = stringIntTableFindEntry(table->entries, table->capacity,
													table->shift, key);
	if (entry->key == NULL)
		return false;

	*value = entry->value;
	return true;
}

bool stringIntTableSet(RunCtx *runCtx, StringIntTable *table, ObjString *key, int32_t value,
					   EloxError *error);
void markStringIntTable(RunCtx *runCtx, StringIntTable *table);

#endif // ELOX_STRING_INT_TABLE_H
