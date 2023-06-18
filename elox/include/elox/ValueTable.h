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
	union {
		uint32_t hash;
	};
} TableEntry;

typedef struct {
	int32_t *chains;
	TableEntry *entries;
	int32_t indexSize;
	int32_t dataSize;
	uint32_t indexShift;
	int32_t fullCount; // includes deleted entries
	int32_t liveCount;
	uint32_t modCount;
} ValueTable;

void initValueTable(ValueTable *table);
void freeValueTable(VMCtx *vmCtx, ValueTable *table);
bool valueTableGet(ValueTable *table, Value key, Value *value, Error *error);
bool valueTableContains(ValueTable *table, Value key, Error *error);
int32_t valueTableGetNext(ValueTable *table, int32_t start, TableEntry **valueEntry);
bool valueTableSet(ValueTable *table, Value key, Value value, Error *error);
bool valueTableDelete(ValueTable *table, Value key, Error *error);
void markValueTable(VMCtx *vmCtx, ValueTable *table);

#endif // ELOX_CLOSE_TABLE_H
