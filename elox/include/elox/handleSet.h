#ifndef ELOX_HANDLE_SET_H
#define ELOX_HANDLE_SET_H

#include "elox/elox-internal.h"

typedef struct {
	EloxHandle *head;
} HandleSet;

void initHandleSet(VMCtx *vmCtx, HandleSet *set);
void freeHandleSet(VMCtx *vmCtx, HandleSet *set);
void handleSetAdd(HandleSet *set, EloxHandle *handle);
void handleSetRemove(VMCtx *vmCtx, EloxHandle *handle);
void markHandleSet(VMCtx *vmCtx, HandleSet *set);

#endif // ELOX_HANDLE_SET_H
