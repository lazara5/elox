#ifndef SLOX_VALUE_TABLE_H
#define SLOX_VALUE_TABLE_H

#include "common.h"
#include "value.h"

typedef struct ExecContext ExecContext;

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
bool valueTableGet(ExecContext *execCtx, ValueTable *table, Value key, Value *value);
int valueTableGetNext(ValueTable *table, int start, ValueEntry **valueEntry);
bool valueTableSet(ExecContext *execCtx, ValueTable *table, Value key, Value value);
bool valueTableDelete(ExecContext *execCtx, ValueTable *table, Value key);
void markValueTable(VMCtx *vmCtx, ValueTable *table);

uint32_t hashValue(ExecContext *execCtx, Value value);

#endif // SLOX_VALUE_TABLE_H
