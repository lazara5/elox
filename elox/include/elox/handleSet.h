#ifndef ELOX_HANDLE_SET_H
#define ELOX_HANDLE_SET_H

#include "elox/elox-internal.h"

typedef struct {
	EloxHandle *handle;
	uintptr_t hash;
} HandleSetEntry;

typedef struct {
	int count;
	int capacity;
	HandleSetEntry *entries;
} HandleSet;

void initHandleSet(HandleSet *set);
void freeHandleSet(VMCtx *vmCtx, HandleSet *set);
void handleSetAdd(VMCtx *vmCtx, HandleSet *set, EloxHandle *handle);
void handleSetDelete(HandleSet *set, EloxHandle *handle);
void markHandleSet(VMCtx *vmCtx, HandleSet *set);

#endif // ELOX_HANDLE_SET_H
