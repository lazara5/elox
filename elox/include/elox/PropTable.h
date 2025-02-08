// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_PROP_TABLE_H
#define ELOX_PROP_TABLE_H

#include <elox/value.h>
#include <elox/object.h>

typedef enum {
	ELOX_PROP_NONE   = 0,
	// Class props
	ELOX_PROP_METHOD = 1 << 0,
	ELOX_PROP_STATIC = 1 << 1,
	// Instance props
	ELOX_PROP_FIELD  = 1 << 2
} ELOX_PACKED PropType;

static const uint32_t ELOX_PROP_ANY = ELOX_PROP_METHOD | ELOX_PROP_STATIC | ELOX_PROP_FIELD;

typedef struct {
	unsigned int index : 24;
	PropType type : 8;
} PropInfo;

typedef struct {
	ObjString *key;
	PropInfo value;
} PropEntry;

typedef struct PropTable {
	int count;
	int capacity;
	uint32_t shift;
	PropEntry *entries;
} PropTable;

void initPropTable(PropTable *table);
void freePropTable(RunCtx *runCtx, PropTable *table);

static inline PropEntry *propTableFindEntry(PropEntry *entries, int capacity,
											uint32_t shift, ObjString *key) {
	uint32_t index = tableIndexFor(key->hash, shift);

	for (;;) {
		PropEntry *entry = &entries[index];
		if (entry->key == NULL) {
			return entry;
		} else if (entry->key == key) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

static inline PropInfo propTableGet(PropTable *table, ObjString *key, uint32_t mask) {
	if (table->count == 0)
		return (PropInfo){ 0, ELOX_PROP_NONE };

	PropEntry *entry = propTableFindEntry(table->entries, table->capacity,
										  table->shift, key);

	// note: empty entries are 0-initialized, so no need to check for NULL key
	return ((entry->value.type & mask) == 0) ?
		(PropInfo){ 0, ELOX_PROP_NONE } : entry->value ;

	return entry->value;
}

static inline PropInfo propTableGetAny(PropTable *table, ObjString *key) {
	if (table->count == 0)
		return (PropInfo){ 0, ELOX_PROP_NONE };

	PropEntry *entry = propTableFindEntry(table->entries, table->capacity,
										  table->shift, key);

	return ((entry->key) == NULL) ?
		(PropInfo){ 0, ELOX_PROP_NONE } : entry->value ;

	return entry->value;
}

bool propTableSet(RunCtx *runCtx, PropTable *table, ObjString *key, PropInfo value,
				  EloxError *error);

void markPropTable(RunCtx *runCtx, PropTable *table);

#endif // ELOX_PROP_TABLE_H
