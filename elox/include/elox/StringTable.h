#ifndef ELOX_STRING_TABLE_H
#define ELOX_STRING_TABLE_H

// Based on the deterministic hash table described by Jason Orendorff
// (see https://wiki.mozilla.org/User:Jorend/Deterministic_hash_tables).
// Originally attributed to Tyler Close

#include <elox/value.h>

#define EST_TOMBSTONE ((ObjString *)UINTPTR_MAX)

typedef struct ExecContext ExecContext;

typedef struct {
	ObjString *key;
	Value value;
	int32_t next;
} StringTableEntry;

typedef struct {
	int32_t *chains;
	StringTableEntry *entries;
	int32_t indexSize;
	int32_t dataSize;
	uint32_t indexShift;
	int32_t fullCount; // includes deleted entries
	int32_t liveCount;
} StringTable;

void initStringTable(StringTable *table);
void freeStringTable(RunCtx *runCtx, StringTable *table);
bool stringTableGet(StringTable *table, ObjString *key, Value *value);
int32_t stringTableGetIndex(StringTable *table, ObjString *key);
bool stringTableContains(StringTable *table, ObjString *key);
bool stringTableSet(RunCtx *runCtx, StringTable *table, ObjString *key, Value value, EloxError *error);
void stringTableAddAll(RunCtx *runCtx, StringTable *from, StringTable *to, EloxError *error);
bool stringTableDelete(StringTable *table, ObjString *key);
void markStringTable(RunCtx *runCtx, StringTable *table);

#endif // ELOX_STRING_TABLE_H
