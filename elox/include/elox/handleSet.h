#ifndef ELOX_HANDLE_SET_H
#define ELOX_HANDLE_SET_H

#include "elox/elox-internal.h"

typedef struct {
	EloxHandle *head;
} HandleSet;

void initHandleSet(HandleSet *set);
void freeHandleSet(VMCtx *vmCtx, HandleSet *set);
void handleSetAdd(HandleSet *set, EloxHandle *handle);
void handleSetRemove(VMCtx *vmCtx, HandleSet *set, EloxHandle *handle);
void markHandleSet(VMCtx *vmCtx, HandleSet *set);

#endif // ELOX_HANDLE_SET_H
