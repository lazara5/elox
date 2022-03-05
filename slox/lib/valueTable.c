#include <stdlib.h>
#include <string.h>

#include "slox/memory.h"
#include "slox/object.h"
#include "slox/valueTable.h"
#include "slox/state.h"

#define TABLE_MAX_LOAD 0.75

void initValueTable(ValueTable *table) {
	table->count = 0;
	table->capacity = 0;
	table->modCount = 0;
	table->entries = NULL;
}

void freeValueTable(VMCtx *vmCtx, ValueTable *table) {
	FREE_ARRAY(vmCtx, Entry, table->entries, table->capacity);
	initValueTable(table);
}

static uint32_t instanceHash(VMCtx *vmCtx, ExecContext *execCtx, ObjInstance *instance) {
	VM *vm = &vmCtx->vm;
	ObjClass *clazz = instance->clazz;
	if (!IS_NIL(clazz->hashCode)) {
		ObjBoundMethod *boundHashCode = newBoundMethod(vmCtx, OBJ_VAL(instance),
													   AS_OBJ(clazz->hashCode));
		push(vm, OBJ_VAL(boundHashCode));
		Value hash = doCall(vmCtx, 0);
		if (!IS_EXCEPTION(hash)) {
			popn(vm, 2);
			return AS_NUMBER(hash);
		}
		execCtx->error = true;
		return 0;
	}
	return instance->identityHash;
}

#ifdef ENABLE_NAN_BOXING

uint32_t hashValue(VMCtx *vmCtx, ExecContext *execCtx, Value value) {
	if (IS_OBJ(value)) {
		Obj *obj = AS_OBJ(value);
		switch (obj->type) {
			case OBJ_STRING:
				return hashString(((ObjString *)obj)->chars, ((ObjString *)obj)->length);
			case OBJ_INSTANCE:
				return instanceHash(vmCtx, execCtx, (ObjInstance *)obj);
			default:
				return 0;
		}
	} else {
		return 0;
	}
}

static bool valuesEquals(const Value a, const Value b) {
	if (IS_STRING(a) && IS_STRING(b)) {
		ObjString *as = AS_STRING(a);
		ObjString *bs = AS_STRING(b);
		return as == bs;
	} else if (IS_NUMBER(a) && IS_NUMBER(b)) {
		return AS_NUMBER(a) == AS_NUMBER(b);
	} else
		return false;
}

#else

uint32_t hashValue(VMCtx *vmCtx, ExecContext *execCtx, Value value) {
	switch (value.type) {
		case VAL_OBJ: {
			Obj *obj = AS_OBJ(value);
			switch (obj->type) {
				case OBJ_STRING:
					return hashString(((ObjString *)obj)->chars, ((ObjString *)obj)->length);
				case OBJ_INSTANCE:
					return instanceHash(vmCtx, execCtx, (ObjInstance *)obj);
				default:
					return 0;
			}
			break;
		}
		default:
			return 0;
	}
}

static bool valuesEquals(const Value a, const Value b) {
	if (a.type != b.type)
		return false;

	switch (a.type) {
		case VAL_BOOL:
			return AS_BOOL(a) == AS_BOOL(b);
		case VAL_NIL:
			return true;
		case VAL_NUMBER:
			return AS_NUMBER(a) == AS_NUMBER(b);
		case VAL_OBJ: {
			Obj *ao = AS_OBJ(a);
			Obj *bo = AS_OBJ(b);
			if (ao->type != bo->type) {
				return false;
			}
			return ao == bo;
		}
		default:
			return false; // Unreachable.
	}
}
#endif // ENABLE_NAN_BOXING

static ValueEntry *findEntry(ValueEntry *entries, int capacity, Value key, uint32_t keyHash) {
	uint32_t index = keyHash & (capacity - 1);
	ValueEntry* tombstone = NULL;

	for (;;) {
		ValueEntry *entry = &entries[index];
		if (IS_NIL(entry->key)) {
			if (IS_NIL(entry->value)) {
				// Empty entry.
				return tombstone != NULL ? tombstone : entry;
			} else {
				// We found a tombstone.
				if (tombstone == NULL) tombstone = entry;
			}
		} else if (valuesEquals(entry->key, key)) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

bool valueTableGet(ValueTable *table, Value key, uint32_t keyHash, Value *value) {
	if (table->count == 0)
		return false;

	ValueEntry *entry = findEntry(table->entries, table->capacity, key, keyHash);
	if (IS_NIL(entry->key))
		return false;

	*value = entry->value;
	return true;
}

int valueTableGetNext(ValueTable *table, int start, ValueEntry **valueEntry) {
	if (start < 0) {
		return -1;
	}

	for (int i = start; i < table->capacity; i++) {
		ValueEntry *entry = &table->entries[i];
		if (!IS_NIL(entry->key)) {
			*valueEntry = entry;
			return i + 1;
		}
	}

	return -1;
}

static void adjustCapacity(VMCtx *vmCtx, ExecContext *execCtx, ValueTable *table, int capacity) {
	ValueEntry *entries = ALLOCATE(vmCtx, ValueEntry, capacity);
	for (int i = 0; i < capacity; i++) {
		entries[i].key = NIL_VAL;
		entries[i].value = NIL_VAL;
	}

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) {
		ValueEntry *entry = &table->entries[i];
		if (IS_NIL(entry->key)) continue;

		uint32_t keyHash = hashValue(vmCtx, execCtx, entry->key);
		if (execCtx->error)
			return;
		ValueEntry *dest = findEntry(entries, capacity, entry->key, keyHash);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(vmCtx, ValueEntry, table->entries, table->capacity);
	table->entries = entries;
	table->capacity = capacity;
}

bool valueTableSet(VMCtx *vmCtx, ExecContext *execCtx, ValueTable *table, Value key, Value value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		adjustCapacity(vmCtx, execCtx, table, capacity);
		if (execCtx->error)
			return false;
		table->modCount++;
	}

	uint32_t keyHash = hashValue(vmCtx, execCtx, key);
	if (execCtx->error)
		return false;
	ValueEntry *entry = findEntry(table->entries, table->capacity, key, keyHash);
	bool isNewKey = (IS_NIL(entry->key));
	if (isNewKey && IS_NIL(entry->value)) {
		table->count++;
		table->modCount++;
	}

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

bool valueTableDelete(ValueTable *table, Value key, uint32_t keyHash) {
	if (table->count == 0)
		return false;

	// Find the entry
	ValueEntry *entry = findEntry(table->entries, table->capacity, key, keyHash);
	if (IS_NIL(entry->key))
		return false;

	// Place a tombstone in the entry
	entry->key = NIL_VAL;
	entry->value = BOOL_VAL(true);
	table->modCount++;
	return true;
}

void markValueTable(VMCtx *vmCtx, ValueTable *table) {
	for (int i = 0; i < table->capacity; i++) {
		ValueEntry *entry = &table->entries[i];
		markValue(vmCtx, entry->key);
		markValue(vmCtx, entry->value);
	}
}
