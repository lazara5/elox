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

static uint32_t instanceHash(ExecContext *execCtx, ObjInstance *instance) {
	VMCtx *vmCtx = execCtx->vmCtx;
	VM *vm = &vmCtx->vm;
	if (instance->flags & INST_HAS_HASHCODE) {
		ObjClass *clazz = instance->clazz;
		ObjBoundMethod *boundHashCode = newBoundMethod(vmCtx, OBJ_VAL(instance),
													   AS_OBJ(clazz->hashCode));
		push(vm, OBJ_VAL(boundHashCode));
		Value hash = doCall(vmCtx, 0);
		if (SLOX_LIKELY(!IS_EXCEPTION(hash))) {
			popn(vm, 2);
			return AS_NUMBER(hash);
		}
		execCtx->error = true;
		return 0;
	}
	return instance->identityHash;
}

static bool instanceEquals(VMCtx *vmCtx, ExecContext *execCtx,
						   ObjInstance *ai, ObjInstance *bi) {
	VM *vm = &vmCtx->vm;
	if (ai->flags & INST_HAS_EQUALS) {
		if (ai->clazz != bi->clazz)
			return false;
		ObjBoundMethod *boundEquals = newBoundMethod(vmCtx, OBJ_VAL(ai),
													 AS_OBJ(ai->clazz->equals));
		push(vm, OBJ_VAL(boundEquals));
		push(vm, OBJ_VAL(bi));
		Value equals = doCall(vmCtx, 1);
		if (!IS_EXCEPTION(equals)) {
			popn(vm, 2);
			return AS_BOOL(equals);
		}
		execCtx->error = true;
		return false;
	}
	return ai == bi;
}

#ifdef ENABLE_NAN_BOXING

uint32_t hashValue(ExecContext *execCtx, Value value) {
	if (IS_OBJ(value)) {
		Obj *obj = AS_OBJ(value);
		switch (obj->type) {
			case OBJ_STRING:
				return hashString(((ObjString *)obj)->chars, ((ObjString *)obj)->length);
			case OBJ_INSTANCE:
				return instanceHash(execCtx, (ObjInstance *)obj);
			default:
				return 0;
		}
	} else if (IS_BOOL(value))
		return AS_BOOL(value);
	else
		return 0;
}

static bool valuesEquals(ExecContext *execCtx, const Value a, const Value b) {
	if (IS_STRING(a) && IS_STRING(b)) {
		ObjString *as = AS_STRING(a);
		ObjString *bs = AS_STRING(b);
		return as == bs;
	} else if (IS_NUMBER(a) && IS_NUMBER(b)) {
		return AS_NUMBER(a) == AS_NUMBER(b);
	} else if (IS_OBJ(a) && IS_OBJ(b)) {
		Obj *ao = AS_OBJ(a);
		Obj *bo = AS_OBJ(b);
		if (ao->type != bo->type) {
			return false;
		}
		switch (ao->type) {
			case OBJ_INSTANCE:
				return instanceEquals(execCtx->vmCtx, execCtx,
									  (ObjInstance *)ao, (ObjInstance *)bo);
			default:
				return ao == bo;
		}
	} else
		return false;
}

#else

uint32_t hashValue(ExecContext *execCtx, Value value) {
	switch (value.type) {
		case VAL_OBJ: {
			Obj *obj = AS_OBJ(value);
			switch (obj->type) {
				case OBJ_STRING:
					return hashString(((ObjString *)obj)->chars, ((ObjString *)obj)->length);
				case OBJ_INSTANCE:
					return instanceHash(execCtx, (ObjInstance *)obj);
				default:
					return 0;
			}
			break;
		}
		case VAL_BOOL:
			return AS_BOOL(value);
		default:
			return 0;
	}
}

static bool valuesEquals(ExecContext *execCtx, const Value a, const Value b) {
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
			switch (ao->type) {
				case OBJ_INSTANCE:
					return instanceEquals(execCtx->vmCtx, execCtx,
										  (ObjInstance *)ao, (ObjInstance *)bo);
				default:
					return ao == bo;
			}
		}
		default:
			return false; // Unreachable.
	}
}
#endif // ENABLE_NAN_BOXING

static ValueEntry *findEntry(ExecContext *execCtx,
							 ValueEntry *entries, int capacity, Value key) {
	uint32_t keyHash = hashValue(execCtx, key);
	if (SLOX_UNLIKELY(execCtx->error))
		return NULL;
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
				if (tombstone == NULL)
					tombstone = entry;
			}
		} else if (valuesEquals(execCtx, entry->key, key)) {
			// We found the key.
			return entry;
		}

		index = (index + 1) & (capacity - 1);
	}
}

bool valueTableGet(ExecContext *execCtx, ValueTable *table, Value key, Value *value) {
	if (table->count == 0)
		return false;

	ValueEntry *entry = findEntry(execCtx, table->entries, table->capacity, key);
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

static void adjustCapacity(ExecContext *execCtx, ValueTable *table, int capacity) {
	VMCtx *vmCtx = execCtx->vmCtx;
	ValueEntry *entries = ALLOCATE(vmCtx, ValueEntry, capacity);
	for (int i = 0; i < capacity; i++) {
		entries[i].key = NIL_VAL;
		entries[i].value = NIL_VAL;
	}

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) {
		ValueEntry *entry = &table->entries[i];
		if (IS_NIL(entry->key))
			continue;

		ValueEntry *dest = findEntry(execCtx, entries, capacity, entry->key);
		if (SLOX_UNLIKELY(execCtx->error))
			return;
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(vmCtx, ValueEntry, table->entries, table->capacity);
	table->entries = entries;
	table->capacity = capacity;
}

bool valueTableSet(ExecContext *execCtx, ValueTable *table, Value key, Value value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		adjustCapacity(execCtx, table, capacity);
		if (SLOX_UNLIKELY(execCtx->error))
			return false;
		table->modCount++;
	}

	ValueEntry *entry = findEntry(execCtx, table->entries, table->capacity, key);
	if (SLOX_UNLIKELY(execCtx->error))
		return false;
	bool isNewKey = (IS_NIL(entry->key));
	if (isNewKey && IS_NIL(entry->value)) {
		table->count++;
		table->modCount++;
	}

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

bool valueTableDelete(ExecContext *execCtx, ValueTable *table, Value key) {
	if (table->count == 0)
		return false;

	// Find the entry
	ValueEntry *entry = findEntry(execCtx, table->entries, table->capacity, key);
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
