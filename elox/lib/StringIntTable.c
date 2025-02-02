// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include <elox/StringIntTable.h>
#include "elox/value.h"
#include "elox/vm.h"
#include "elox/state.h"

#include <stdlib.h>
#include <string.h>

#define TABLE_MAX_LOAD 0.75

void initStringIntTable(StringIntTable *table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freeStringIntTable(RunCtx *runCtx, StringIntTable *table) {
	FREE_ARRAY(runCtx, Entry, table->entries, table->capacity);
	initStringIntTable(table);
}

static bool adjustCapacity(RunCtx *runCtx, StringIntTable *table, uint32_t newCapacity) {
	StringIntEntry *newEntries = ALLOCATE(runCtx, StringIntEntry, newCapacity);
	if (ELOX_UNLIKELY(newEntries == NULL))
		return false;
	for (uint32_t i = 0; i < newCapacity; i++) {
		newEntries[i].key = NULL;
		newEntries[i].value = 0;
	}

	uint32_t log2Size = ELOX_CTZ(newCapacity);
	uint32_t shift = 8 * sizeof(uint32_t) - log2Size;

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) {
		StringIntEntry *entry = &table->entries[i];
		if (entry->key == NULL)
			continue;

		StringIntEntry *dst = stringIntTableFindEntry(newEntries, newCapacity,
													  shift, entry->key);
		dst->key = entry->key;
		dst->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(runCtx, StringIntEntry, table->entries, table->capacity);
	table->entries = newEntries;
	table->capacity = newCapacity;
	table->shift = shift;

	return true;
}

bool stringIntTableSet(RunCtx *runCtx, StringIntTable *table, ObjString *key, int32_t value,
					   EloxError *error) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		bool adjusted = adjustCapacity(runCtx, table, capacity);
		ELOX_CHECK_THROW_RET_VAL(adjusted, error, OOM(runCtx), false);
	}

	StringIntEntry *entry = stringIntTableFindEntry(table->entries, table->capacity,
													table->shift, key);
	bool isNewKey = (entry->key == NULL);
	if (isNewKey)
		table->count++;

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

void markStringIntTable(RunCtx *runCtx, StringIntTable *table) {
	for (int i = 0; i < table->capacity; i++) {
		StringIntEntry *entry = &table->entries[i];
		markObject(runCtx, (Obj *)entry->key);
	}
}
