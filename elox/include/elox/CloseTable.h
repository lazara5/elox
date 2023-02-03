#ifndef ELOX_CLOSE_TABLE_H
#define ELOX_CLOSE_TABLE_H

// Based on the deterministic hash table described by Jason Orendorff
// (see https://wiki.mozilla.org/User:Jorend/Deterministic_hash_tables).
// Originally attributed to Tyler Close

#include <elox/value.h>

typedef struct ExecContext ExecContext;

typedef struct {
	Value key;
	Value value;
	int32_t next;
	int32_t chain;
} TableEntry;

typedef struct {
	TableEntry *entries;
	int32_t tableSize;
	int32_t entriesCount; // includes deleted entries
	int32_t count;
	uint32_t modCount;
} CloseTable;

void initCloseTable(CloseTable *table);
void freeCloseTable(VMCtx *vmCtx, CloseTable *table);
bool closeTableGet(CloseTable *table, Value key, Value *value, Error *error);
int32_t closeTableGetNext(CloseTable *table, int32_t start, TableEntry **valueEntry);
bool closeTableSet(CloseTable *table, Value key, Value value, Error *error);
void markCloseTable(VMCtx *vmCtx, CloseTable *table);

#endif // ELOX_CLOSE_TABLE_H
