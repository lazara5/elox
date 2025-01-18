// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/StringTable.h>
#include <elox/state.h>

#include <string.h>

// Based on the deterministic hash table described by Jason Orendorff
// (see https://wiki.mozilla.org/User:Jorend/Deterministic_hash_tables).
// Originally attributed to Tyler Close

void initStringTable(StringTable *table) {
	table->liveCount = table->fullCount = 0;
	table->indexSize = table->dataSize = 0;
	table->indexShift = 0;
	table->chains = NULL;
	table->entries = NULL;
}

void freeStringTable(RunCtx *runCtx, StringTable *table) {
	FREE_ARRAY(runCtx, int32_t, table->chains, table->indexSize);
	FREE_ARRAY(runCtx, StringTableEntry, table->entries, table->dataSize);
	initStringTable(table);
}

static int32_t lookup(StringTable *table, ObjString *key) {
	uint32_t bucket = indexFor(key->hash, table->indexShift);

	int32_t idx = table->chains[bucket];
	while (idx >= 0) {
		StringTableEntry *entry = &table->entries[idx];
		if (entry->key != EST_TOMBSTONE) {
			if (entry->key == key)
				return idx;
		}
		idx = entry->next;
	}

	return -1;
}

bool stringTableGet(StringTable *table, ObjString *key, Value *value) {
	if (table->liveCount == 0)
		return false;

	int32_t idx = lookup(table, key);
	if (idx >= 0) {
		*value = table->entries[idx].value;
		return true;
	}

	return false;
}

int32_t stringTableGetIndex(StringTable *table, ObjString *key) {
	if (table->liveCount == 0)
		return -1;

	return lookup(table, key);
}

bool stringTableContains(StringTable *table, ObjString *key) {
	if (table->liveCount == 0)
		return false;

	int32_t idx = lookup(table, key);
	return idx >= 0;
}

static void rehash(RunCtx *runCtx, StringTable *table, int32_t newSize, EloxError *error) {
	if (newSize == 0)
		newSize = 8;

	int32_t *newChains = NULL;
	StringTableEntry *newEntries = NULL;
	int32_t dataSize;

	int32_t indexSize = newSize;

	if (newSize == table->indexSize) {
		// rehash in place
		for (int i = 0; i < indexSize; i++)
			table->chains[i] = -1;
		int j = 0;
		for (int i = 0; i < table->dataSize; table++) {
			StringTableEntry *entry = &table->entries[i];
			if (entry->key != EST_TOMBSTONE) {
				uint32_t keyHash = entry->key->hash;
				if (i != j) {
					memcpy(table->entries + j, table->entries + i, sizeof(StringTableEntry));
					entry = table->entries + j;
				}
				uint32_t bucket = indexFor(keyHash, table->indexShift);
				entry->next = table->chains[bucket];
				table->chains[bucket] = j;
				j++;
			}
		}
		table->fullCount = table->liveCount;
	} else {
		dataSize = (newSize * 3) / 4;  // fill factor: 0.75
		newChains = ALLOCATE(runCtx, int32_t, indexSize);
		if (ELOX_UNLIKELY(newChains == NULL)) {
			oomError(runCtx);
			goto cleanup;
		}
		newEntries = ALLOCATE(runCtx, StringTableEntry, dataSize);
		if (ELOX_UNLIKELY(newEntries == NULL)) {
			oomError(runCtx);
			goto cleanup;
		}

		uint32_t log2Size = ELOX_CTZ(indexSize);
		uint32_t newShift = 8 * sizeof(uint32_t) - log2Size;

		for (int i = 0; i < indexSize; i++)
			newChains[i] = -1;

		StringTableEntry *q = newEntries;
		for (StringTableEntry *p = table->entries, *end = table->entries + table->fullCount; p != end; p++) {
			if (p->key != EST_TOMBSTONE) {
				uint32_t bucket = indexFor(p->key->hash, newShift);
				q->key = p->key;
				q->value = p->value;
				q->next = newChains[bucket];
				newChains[bucket] = q - newEntries;
				q++;
			}
		}

		int32_t *oldChains = table->chains;
		StringTableEntry *oldEntries = table->entries;
		int32_t oldIndexSize = table->indexSize;
		int32_t oldDataSize = table->dataSize;

		table->chains = newChains;
		table->entries = newEntries;
		table->indexSize = indexSize;
		table->dataSize = dataSize;
		table->indexShift = newShift;
		table->fullCount = table->liveCount;

		FREE_ARRAY(runCtx, int32_t, oldChains, oldIndexSize);
		FREE_ARRAY(runCtx, StringTableEntry, oldEntries, oldDataSize);
	}

	return;

cleanup:
	error->raised = true;
	if (newChains != NULL)
		FREE_ARRAY(runCtx, int32_t, newChains, indexSize);
	if (newEntries != NULL)
		FREE_ARRAY(runCtx, StringTableEntry, newEntries, dataSize);
}

bool stringTableSet(RunCtx *runCtx, StringTable *table, ObjString *key, Value value, EloxError *error) {
	if (table->liveCount > 0) {
		int32_t idx = lookup(table, key);
		if (idx >= 0) {
			table->entries[idx].value = value;
			return false;
		}
	}

	if (table->fullCount == table->dataSize) {
		rehash(runCtx, table,
			   table->liveCount >= (table->dataSize * 3) / 4
					? 2 * table->indexSize
					: table->indexSize,
			   error);
		if (ELOX_UNLIKELY(error->raised))
			return false;
	}

	table->liveCount++;
	StringTableEntry *e = &table->entries[table->fullCount++];
	e->key = key;
	e->value = value;
	uint32_t bucket = indexFor(key->hash, table->indexShift);
	e->next = table->chains[bucket];
	table->chains[bucket] = table->fullCount - 1;

	return true;
}

void stringTableAddAll(RunCtx *runCtx, StringTable *from, StringTable *to, EloxError *error) {
	for (int i = 0; i < from->fullCount; i++) {
		StringTableEntry *entry = &from->entries[i];
		if (entry->key != EST_TOMBSTONE) {
			stringTableSet(runCtx, to, entry->key, entry->value, error);
			if (ELOX_UNLIKELY(error->raised))
				return;
		}
	}
}

bool stringTableDelete(StringTable *table, ObjString *key) {
	if (table->liveCount == 0)
		return false;

	int32_t idx = lookup(table, key);
	if (idx < 0)
		return false;

	table->entries[idx].key = EST_TOMBSTONE;
	table->liveCount--;

	return true;
}

void markStringTable(RunCtx *runCtx, StringTable *table) {
	for (int32_t i = 0; i < table->fullCount; i++) {
		StringTableEntry *entry = &table->entries[i];
		if (entry->key != EST_TOMBSTONE) {
			markObject(runCtx, (Obj *)entry->key);
			markValue(runCtx, entry->value);
		}
	}
}
