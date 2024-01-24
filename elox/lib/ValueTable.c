// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/ValueTable.h>
#include <elox/object.h>
#include <elox/state.h>

// Fibonacci hashing, see
// https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
static inline uint32_t indexFor(uint32_t hash, uint32_t shift) {
	return (hash * 2654435769u) >> shift;
}

// Based on the deterministic hash table described by Jason Orendorff
// (see https://wiki.mozilla.org/User:Jorend/Deterministic_hash_tables).
// Originally attributed to Tyler Close

void initValueTable(ValueTable *table) {
	table->liveCount = table->fullCount = 0;
	table->indexSize = table->dataSize = 0;
	table->indexShift = 0;
	table->modCount = 0;
	table->chains = NULL;
	table->entries = NULL;
}

void freeValueTable(VMCtx *vmCtx, ValueTable *table) {
	FREE_ARRAY(vmCtx, int32_t, table->chains, table->indexSize);
	FREE_ARRAY(vmCtx, TableEntry, table->entries, table->dataSize);
	initValueTable(table);
}

static int32_t lookup(ValueTable *table, Value key, uint32_t keyHash, Error *error) {
	uint32_t bucket = indexFor(keyHash, table->indexShift);
	//uint32_t bucket = keyHash & (table->indexSize - 1);

	int32_t idx = table->chains[bucket];
	while (idx >= 0) {
		TableEntry *entry = &table->entries[idx];
		if (!IS_UNDEFINED(key)) {
			if (valuesEquals(entry->key, key, error))
				return idx;
			if (ELOX_UNLIKELY(error->raised))
				return -1;
		}
		idx = entry->next;
	}

	return -1;
}

bool valueTableGet(ValueTable *table, Value key, Value *value, Error *error) {
	if (table->liveCount == 0)
		return false;

	uint32_t keyHash = hashValue(key, error);
	if (ELOX_UNLIKELY(error->raised))
		return false;

	int32_t idx = lookup(table, key, keyHash, error);
	if (idx >= 0) {
		*value = table->entries[idx].value;
		return true;
	}

	return false;
}

bool valueTableContains(ValueTable *table, Value key, Error *error) {
	if (table->liveCount == 0)
		return false;

	uint32_t keyHash = hashValue(key, error);
	if (ELOX_UNLIKELY(error->raised))
		return false;

	int32_t idx = lookup(table, key, keyHash, error);
	return idx >= 0;
}

int32_t valueTableGetNext(ValueTable *table, int32_t start, TableEntry **valueEntry) {
	if (start < 0)
		return -1;

	for (int i = start; i < table->fullCount; i++) {
		TableEntry *entry = &table->entries[i];
		if (!IS_UNDEFINED(entry->key)) {
			*valueEntry = entry;
			return i + 1;
		}
	}

	return -1;
}

static void rehash(ValueTable *table, int32_t newSize, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	if (newSize == 0)
		newSize = 8;

	int32_t *newChains = NULL;
	TableEntry *newEntries = NULL;
	int32_t dataSize;

	int32_t indexSize = newSize;

	if (newSize == table->indexSize) {
		// rehash in place
		for (int i = 0; i < indexSize; i++)
			table->chains[i] = -1;
		int j = 0;
		for (int i = 0; i < table->dataSize; table++) {
			TableEntry *entry = &table->entries[i];
			uint32_t keyHash = entry->hash;
			if (!IS_UNDEFINED(entry->key)) {
				if (i != j) {
					memcpy(table->entries + j, table->entries + i, sizeof(TableEntry));
					entry = table->entries + j;
				}
				uint32_t bucket = indexFor(keyHash, table->indexShift);
				//uint32_t bucket = keyHash & (indexSize - 1);
				entry->next = table->chains[bucket];
				table->chains[bucket] = j;
				j++;
			}
		}
		table->fullCount = table->liveCount;
	} else {
		dataSize = (newSize * 3) / 4;  // fill factor: 0.75
		newChains = ALLOCATE(vmCtx, int32_t, indexSize);
		if (ELOX_UNLIKELY(newChains == NULL)) {
			oomError(vmCtx);
			goto cleanup;
		}
		newEntries = ALLOCATE(vmCtx, TableEntry, dataSize);
		if (ELOX_UNLIKELY(newEntries == NULL)) {
			oomError(vmCtx);
			goto cleanup;
		}

		uint32_t log2Size = ELOX_CTZ(indexSize);
		uint32_t newShift = 8 * sizeof(uint32_t) - log2Size;

		for (int i = 0; i < indexSize; i++)
			newChains[i] = -1;

		TableEntry *q = newEntries;
		for (TableEntry *p = table->entries, *end = table->entries + table->fullCount; p != end; p++) {
			if (!IS_UNDEFINED(p->key)) {
				uint32_t keyHash = p->hash;
				uint32_t bucket = indexFor(keyHash, newShift);
				//uint32_t bucket = keyHash & (indexSize - 1);
				q->key = p->key;
				q->value = p->value;
				q->next = newChains[bucket];
				q->hash = keyHash;
				newChains[bucket] = q - newEntries;
				q++;
			}
		}

		int32_t *oldChains = table->chains;
		TableEntry *oldEntries = table->entries;
		int32_t oldIndexSize = table->indexSize;
		int32_t oldDataSize = table->dataSize;

		table->chains = newChains;
		table->entries = newEntries;
		table->indexSize = indexSize;
		table->dataSize = dataSize;
		table->indexShift = newShift;
		table->fullCount = table->liveCount;

		FREE_ARRAY(vmCtx, int32_t, oldChains, oldIndexSize);
		FREE_ARRAY(vmCtx, TableEntry, oldEntries, oldDataSize);
	}

	return;

cleanup:
	error->raised = true;
	if (newChains != NULL)
		FREE_ARRAY(vmCtx, int32_t, newChains, indexSize);
	if (newEntries != NULL)
		FREE_ARRAY(vmCtx, TableEntry, newEntries, dataSize);
}

bool valueTableSet(ValueTable *table, Value key, Value value, Error *error) {
	uint32_t keyHash = hashValue(key, error);
	if (ELOX_UNLIKELY(error->raised))
		return false;

	if (table->liveCount > 0) {
		int32_t idx = lookup(table, key, keyHash, error);
		if (ELOX_UNLIKELY(error->raised))
			return false;
		if (idx >= 0) {
			table->entries[idx].value = value;
			return false;
		}
	}

	table->modCount++;

	if (table->fullCount == table->dataSize) {
		rehash(table,
			   table->liveCount >= (table->dataSize * 3) / 4
					? 2 * table->indexSize
					: table->indexSize,
			   error);
		if (ELOX_UNLIKELY(error->raised))
			return false;
	}

	table->liveCount++;
	TableEntry *e = &table->entries[table->fullCount++];
	e->key = key;
	e->value = value;
	e->hash = keyHash;
	uint32_t bucket = indexFor(keyHash, table->indexShift);
	//uint32_t bucket = keyHash & (table->indexSize - 1);
	e->next = table->chains[bucket];
	table->chains[bucket] = table->fullCount - 1;

	return true;
}

bool valueTableDelete(ValueTable *table, Value key, Error *error) {
	if (table->liveCount == 0)
		return false;

	uint32_t keyHash = hashValue(key, error);
	if (ELOX_UNLIKELY(error->raised))
		return false; // TODO

	int32_t idx = lookup(table, key, keyHash, error);
	if (idx < 0)
		return false;

	table->modCount++;

	table->entries[idx].key = UNDEFINED_VAL;
	table->liveCount--;

	return true;
}

void markValueTable(VMCtx *vmCtx, ValueTable *table) {
	for (int32_t i = 0; i < table->fullCount; i++) {
		TableEntry *entry = &table->entries[i];
		if (!IS_UNDEFINED(entry->key)) {
			markValue(vmCtx, entry->key);
			markValue(vmCtx, entry->value);
		}
	}
}
