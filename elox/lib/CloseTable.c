#include <elox/CloseTable.h>
#include <elox/object.h>
#include <elox/state.h>

void initCloseTable(CloseTable *table) {
	table->count = 0;
	table->entriesCount = 0;
	table->entriesCapacity = 0;
	table->tableSize = 0;
	table->modCount = 0;
	table->table = NULL;
	table->entries = NULL;
}

void freeCloseTable(VMCtx *vmCtx, CloseTable *table) {
	FREE_ARRAY(vmCtx, int32_t, table->table, table->tableSize);
	FREE_ARRAY(vmCtx, TableEntry, table->entries, table->entriesCapacity);
	initCloseTable(table);
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
			pop(vm);
			return AS_BOOL(equals);
		}
		execCtx->error = true;
		return false;
	}
	return ai == bi;
}


#ifdef ELOX_ENABLE_NAN_BOXING

static bool valuesEquals(ExecContext *execCtx, const Value a, const Value b) {
	if (IS_STRING(a) && IS_STRING(b)) {
		ObjString *as = AS_STRING(a);
		ObjString *bs = AS_STRING(b);
		return as == bs;
	} else if (IS_NUMBER(a) && IS_NUMBER(b))
		return AS_NUMBER(a) == AS_NUMBER(b);
	else if (IS_OBJ(a) && IS_OBJ(b)) {
		Obj *ao = AS_OBJ(a);
		Obj *bo = AS_OBJ(b);
		if (ao->type != bo->type)
			return false;
		switch (ao->type) {
			case OBJ_INSTANCE:
				return instanceEquals(execCtx->vmCtx, execCtx,
									  (ObjInstance *)ao, (ObjInstance *)bo);
			case OBJ_STRINGPAIR: {
				ObjStringPair *pair1 = (ObjStringPair *)ao;
				ObjStringPair *pair2 = (ObjStringPair *)bo;
				return (pair1->str1 == pair2->str1) && (pair1->str2 == pair2->str2);
			}
			default:
				return ao == bo;
		}
	} else
		return false;
}

#else

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
				case OBJ_STRINGPAIR: {
					ObjStringPair *pair1 = (ObjStringPair *)ao;
					ObjStringPair *pair2 = (ObjStringPair *)bo;
					return (pair1->str1 == pair2->str1) && (pair1->str2 == pair2->str2);
				}
				default:
					return ao == bo;
			}
		}
		default:
			return false; // Unreachable.
	}
}
#endif // ELOX_ENABLE_NAN_BOXING

static TableEntry *lookup(ExecContext *execCtx, CloseTable *table, Value key, uint32_t keyHash) {
	uint32_t bucket = keyHash & (table->tableSize - 1);

	int32_t idx = table->table[bucket];
	while (idx >= 0) {
		TableEntry *entry = &table->entries[idx];
		if (!IS_UNDEFINED(key)) {
			if (valuesEquals(execCtx, entry->key, key))
				return entry;
		}
		idx = entry->next;
	}

	return NULL;
}

bool closeTableGet(ExecContext *execCtx, CloseTable *table, Value key, Value *value) {
	if (table->count == 0)
		return false;

	uint32_t keyHash = hashValue(execCtx, key);
	if (ELOX_UNLIKELY(execCtx->error))
		return false; // TODO

	TableEntry *e = lookup(execCtx, table, key, keyHash);
	if (e != NULL) {
		*value = e->value;
		return true;
	}

	return false;
}

int32_t closeTableGetNext(CloseTable *table, int32_t start, TableEntry **valueEntry) {
	if (start < 0) {
		return -1;
	}

	for (int i = start; i < table->entriesCapacity; i++) {
		TableEntry *entry = &table->entries[i];
		if (!IS_UNDEFINED(entry->key)) {
			*valueEntry = entry;
			return i + 1;
		}
	}

	return -1;
}


static void rehash(ExecContext *execCtx, CloseTable *table, int32_t newSize) {
	VMCtx *vmCtx = execCtx->vmCtx;

	if (newSize == 0)
		newSize = 8;

	if (newSize == table->tableSize) {

	} else {
		int32_t *newTable = ALLOCATE(vmCtx, int32_t, newSize);
		memset(newTable, 0xff, newSize * sizeof(int32_t));
		TableEntry *newEntries = ALLOCATE(vmCtx, TableEntry, newSize);

		TableEntry *q = newEntries;
		for (TableEntry *p = table->entries, *end = table->entries + table->entriesCapacity; p != end; p++) {
			if (!IS_UNDEFINED(p->key)) {
				uint32_t keyHash = hashValue(execCtx, p->key);
				if (ELOX_UNLIKELY(execCtx->error))
					return; // TODO: propagate
				uint32_t bucket = keyHash & (newSize - 1);
				q->key = p->key;
				q->value = p->value;
				q->next = newTable[bucket];
				newTable[bucket] = q - newEntries;
				q++;
			}
		}

		int32_t *oldTable = table->table;
		TableEntry *oldEntries = table->entries;
		int32_t oldSize = table->tableSize;
		int32_t oldEntriesCapacity = table->entriesCapacity;

		table->table = newTable;
		table->entries = newEntries;
		table->tableSize = newSize;
		table->entriesCapacity = newSize;
		table->entriesCount = table->count;

		FREE_ARRAY(vmCtx, int32_t, oldTable, oldSize);
		FREE_ARRAY(vmCtx, TableEntry, oldEntries, oldEntriesCapacity);
	}
}

bool closeTableSet(ExecContext *execCtx, CloseTable *table, Value key, Value value) {
	// TODO: propagate exception
	uint32_t keyHash = hashValue(execCtx, key);
	if (ELOX_UNLIKELY(execCtx->error))
		return false;

	TableEntry *e = NULL;
	if (table->count > 0) {
		TableEntry *e = lookup(execCtx, table, key, keyHash);
		if (e != NULL) {
			e->value = value;
			return false;
		}
	}

	table->modCount++;

	if (table->entriesCount == table->entriesCapacity) {
		rehash(execCtx, table,
			   table->count >= table->entriesCapacity * 0.75
					? 2 * table->tableSize
					: table->tableSize);
	}

	table->count++;
	e = &table->entries[table->entriesCount++];
	e->key = key;
	e->value = value;
	uint32_t bucket = keyHash & (table->tableSize - 1);
	e->next = table->table[bucket];
	table->table[bucket] = table->entriesCount - 1;

	return true;
}

bool closeTableDelete(ExecContext *execCtx, CloseTable *table, Value key) {
	if (table->count == 0)
		return false;

	uint32_t keyHash = hashValue(execCtx, key);
	if (ELOX_UNLIKELY(execCtx->error))
		return false; // TODO

	TableEntry *e = lookup(execCtx, table, key, keyHash);
	if (e == NULL)
		return false;

	table->modCount++;

	e->key = UNDEFINED_VAL;
	table->count--;

	return true;
}

void markCloseTable(VMCtx *vmCtx, CloseTable *table) {
	for (int32_t i = 0; i < table->entriesCapacity; i++) {
		TableEntry *entry = &table->entries[i];
		if (!IS_UNDEFINED(entry->key)) {
			markValue(vmCtx, entry->key);
			markValue(vmCtx, entry->value);
		}
	}
}
