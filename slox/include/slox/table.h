#ifndef SLOX_TABLE_H
#define SLOX_TABLE_H

#include "common.h"
#include "value.h"

typedef struct {
	ObjString *key;
	Value value;
} Entry;

typedef struct {
	int count;
	int capacity;
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
ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash);
void tableRemoveWhite(Table *table);
void markTable(VMCtx *vmCtx, Table *table);

#endif // SLOX_TABLE_H
