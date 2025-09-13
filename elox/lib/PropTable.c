// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include <elox/PropTable.h>
#include "elox/value.h"
#include "elox/vm.h"
#include "elox/state.h"

#include <stdlib.h>
#include <string.h>

#define TABLE_MAX_LOAD 0.75

void initPropTable(PropTable *table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freePropTable(VMCtx *vmCtx, PropTable *table) {
	FREE_ARRAY(vmCtx, Entry, table->entries, table->capacity);
	initPropTable(table);
}

static bool adjustCapacity(RunCtx *runCtx, PropTable *table, uint32_t newCapacity) {
	VMCtx *vmCtx = runCtx->vmCtx;

	PropEntry *newEntries = ALLOCATE(runCtx, PropEntry, newCapacity);
	if (ELOX_UNLIKELY(newEntries == NULL))
		return false;
	for (uint32_t i = 0; i < newCapacity; i++) {
		newEntries[i].key = NULL;
		newEntries[i].value = (PropInfo){ 0 };
	}

	uint32_t log2Size = ELOX_CTZ(newCapacity);
	uint32_t shift = 8 * sizeof(uint32_t) - log2Size;

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) {
		PropEntry *entry = &table->entries[i];
		if (entry->key == NULL)
			continue;

		PropEntry *dst = propTableFindEntry(newEntries, newCapacity,
											shift, entry->key);
		dst->key = entry->key;
		dst->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(vmCtx, PropEntry, table->entries, table->capacity);
	table->entries = newEntries;
	table->capacity = newCapacity;
	table->shift = shift;

	return true;
}

bool propTableSet(RunCtx *runCtx, PropTable *table, ObjString *key, PropInfo value,
				  EloxError *error) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		bool adjusted = adjustCapacity(runCtx, table, capacity);
		ELOX_CHECK_RAISE_RET_VAL(adjusted, error, OOM(runCtx), false);
	}

	PropEntry *entry = propTableFindEntry(table->entries, table->capacity,
										  table->shift, key);
	bool isNewKey = (entry->key == NULL);
	if (isNewKey)
		table->count++;

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

void markPropTable(VMCtx *vmCtx, PropTable *table) {
	for (int i = 0; i < table->capacity; i++) {
		PropEntry *entry = &table->entries[i];
		markObject(vmCtx, (Obj *)entry->key);
	}
}
