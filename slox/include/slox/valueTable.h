#ifndef SLOX_VALUE_TABLE_H
#define SLOX_VALUE_TABLE_H

#include "common.h"
#include "value.h"

typedef struct {
	Value key;
	Value value;
} ValueEntry;

typedef struct {
	ValueEntry *entries;
	int count;
	int capacity;
	int modCount;
} ValueTable;

void initValueTable(ValueTable *table);
void freeValueTable(VMCtx *vmCtx, ValueTable *table);
bool valueTableGet(ValueTable *table, Value key, Value *value);
bool valueTableSet(VMCtx *vmCtx, ValueTable *table, Value key, Value value);
bool valueTableDelete(ValueTable *table, Value key);
void valueTableRemoveWhite(ValueTable *table);
void markValueTable(VMCtx *vmCtx, ValueTable *table);

#endif // SLOX_VALUE_TABLE_H
