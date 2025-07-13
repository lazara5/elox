// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_PROP_TABLE_H
#define ELOX_PROP_TABLE_H

#include <elox/value.h>
#include <elox/object.h>

typedef enum {
#define ELOX_PROP_TYPES_INLINE
#define PROP_TYPE(name) ELOX_PROP_##name,
#include "propTypes.h"
#undef PROP_TYPE
#undef ELOX_PROP_TYPES_INLINE
} ELOX_PACKED PropType;

static const uint8_t ELOX_PROP_METHOD_MASK = 1 << (ELOX_PROP_METHOD - 1);
static const uint8_t ELOX_PROP_STATIC_MASK = 1 << (ELOX_PROP_STATIC - 1);
static const uint8_t ELOX_PROP_FIELD_MASK = 1 << (ELOX_PROP_FIELD - 1);

static const uint8_t ELOX_PROP_ANY_MASK =
	ELOX_PROP_METHOD_MASK |
	ELOX_PROP_STATIC_MASK |
	ELOX_PROP_FIELD_MASK;

typedef struct {
	uint16_t index;
	PropType type;
	uint8_t mask;
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

static const PropInfo NO_PROP_INFO = (PropInfo) {
	.index = 0,
	.type = ELOX_PROP_NONE,
	.mask = 0
};

static inline PropInfo propTableGet(PropTable *table, ObjString *key, uint8_t mask) {
	if (table->count == 0)
		return (PropInfo){ 0, ELOX_PROP_NONE, 0 };

	PropEntry *entry = propTableFindEntry(table->entries, table->capacity,
										  table->shift, key);

	// note: empty entries are 0-initialized, so no need to check for NULL key
	return ((entry->value.mask & mask) == 0) ?
		NO_PROP_INFO : entry->value ;

	return entry->value;
}

static inline PropInfo propTableGetAny(PropTable *table, ObjString *key) {
	if (table->count == 0)
		return NO_PROP_INFO;

	PropEntry *entry = propTableFindEntry(table->entries, table->capacity,
										  table->shift, key);

	return ((entry->key) == NULL) ? NO_PROP_INFO : entry->value ;

	return entry->value;
}

bool propTableSet(RunCtx *runCtx, PropTable *table, ObjString *key, PropInfo value,
				  EloxError *error);

void markPropTable(RunCtx *runCtx, PropTable *table);

#endif // ELOX_PROP_TABLE_H
