#include "elox/memory.h"
#include "elox/object.h"
#include "elox/elox-internal.h"
#include "elox/value.h"
#include "elox/handleSet.h"

#include <stdlib.h>

#define SET_MAX_LOAD 0.7
#define TOMBSTONE_MARKER_VALUE 0x1
#define TOMBSTONE_MARKER ((EloxHandle *)TOMBSTONE_MARKER_VALUE)

// based on fmix64/fmix32 from MurmurHash3

#if UINTPTR_MAX == UINT64_MAX
inline static uintptr_t ptrHash(void *ptr) {
	uintptr_t uPtr = (uintptr_t)ptr;
	uPtr ^= uPtr >> 33;
	uPtr *= 0xff51afd7ed558ccd;
	uPtr ^= uPtr >> 33;
	uPtr *= 0xc4ceb9fe1a85ec53;
	uPtr ^= uPtr >> 33;
	return uPtr;
};
#elif INTPTR_MAX == INT32_MAX
inline static uintptr_t ptrHash(void *ptr) {
	uintptr_t uPtr = (uintptr_t)ptr;
	uPtr ^= uPtr >> 16;
	uPtr *= 0x85ebca6b;
	uPtr ^= uPtr >> 13;
	uPtr *= 0xc2b2ae35;
	uPtr ^= uPtr >> 16;
	return uPtr;
};
#endif

void initHandleSet(HandleSet *set) {
	set->count = 0;
	set->capacity = 0;
	set->entries = NULL;
}

void freeHandleSet(VMCtx *vmCtx, HandleSet *set) {
	FREE_ARRAY(vmCtx, HandleSetEntry, set->entries, set->capacity);
	initHandleSet(set);
}

static HandleSetEntry *findEntry(HandleSetEntry *entries, uintptr_t capacity,
								 EloxHandle *key, uintptr_t hash) {
	uint32_t index = hash & (capacity - 1);
	HandleSetEntry *tombstone = NULL;

	for (;;) {
		HandleSetEntry *entry = &entries[index];
		if (entry->handle == NULL) {
			return tombstone != NULL ? tombstone : entry;
		} else if (entry->handle == TOMBSTONE_MARKER) {
			if (tombstone == NULL)
				tombstone = entry;
		} else if (entry->handle == key) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

static void adjustCapacity(VMCtx *vmCtx, HandleSet *set, int capacity) {
	HandleSetEntry *entries = ALLOCATE(vmCtx, HandleSetEntry, capacity);
	for (int i = 0; i < capacity; i++)
		entries[i].handle = NULL;

	set->count = 0;
	for (int i = 0; i < set->capacity; i++) {
		HandleSetEntry *entry = &set->entries[i];
		if ((uintptr_t)entry->handle <= TOMBSTONE_MARKER_VALUE)
			continue;

		HandleSetEntry *dest = findEntry(entries, capacity, entry->handle, entry->hash);
		dest->handle = entry->handle;
		set->count++;
	}

	FREE_ARRAY(vmCtx, HandleSetEntry, set->entries, set->capacity);
	set->entries = entries;
	set->capacity = capacity;
}

void handleSetAdd(VMCtx *vmCtx, HandleSet *set, EloxHandle *handle) {
	if (set->count + 1 > set->capacity * SET_MAX_LOAD) {
		int capacity = GROW_CAPACITY(set->capacity);
		adjustCapacity(vmCtx, set, capacity);
	}

	uintptr_t hash = ptrHash(handle);
	HandleSetEntry *entry = findEntry(set->entries, set->capacity, handle, hash);
	if (entry->handle == NULL)
		set->count++;

	entry->handle = handle;
	entry->hash = hash;
}

void handleSetDelete(HandleSet *set, EloxHandle *handle) {
	if (set->count == 0)
		return;

	// Find the entry
	uintptr_t hash = ptrHash(handle);
	HandleSetEntry *entry = findEntry(set->entries, set->capacity, handle, hash);
	if ((uintptr_t)entry->handle <= TOMBSTONE_MARKER_VALUE)
		return;

	// Place a tombstone in the entry
	entry->handle = TOMBSTONE_MARKER;
}

void markHandleSet(VMCtx *vmCtx, HandleSet *set) {
	for (int i = 0; i < set->capacity; i++) {
		HandleSetEntry *entry = &set->entries[i];
		if ((uintptr_t)entry->handle > TOMBSTONE_MARKER_VALUE)
			markHandle(vmCtx, entry->handle);
	}
}
