// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_TABLE_H
#define ELOX_TABLE_H

#include "value.h"

typedef struct {
	ObjString *key;
	Value value;
} Entry;

typedef struct {
	int count;
	int capacity;
	uint32_t shift;
	Entry *entries;
} Table;

void initTable(Table *table);
void freeTable(VMCtx *vmCtx, Table *table);
bool tableGet(Table *table, ObjString *key, Value *value);
int tableGetIndex(Table *table, ObjString *key);
bool tableSet(VMCtx *vmCtx, Table *table, ObjString *key, Value value);
Value tableSetIfMissing(VMCtx *vmCtx, Table *table, ObjString *key, Value value);
bool tableDelete(Table *table, ObjString *key);
void tableAddAll(VMCtx *vmCtx, Table *from, Table *to);
ObjString *tableFindString(Table *table, const uint8_t *chars, int length, uint32_t hash);
void tableRemoveWhite(Table *table);
void markTable(VMCtx *vmCtx, Table *table);

#endif // ELOX_TABLE_H
