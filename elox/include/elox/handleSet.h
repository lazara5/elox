// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_HANDLE_SET_H
#define ELOX_HANDLE_SET_H

#include "elox/elox-internal.h"

typedef struct {
	EloxHandle *head;
} HandleSet;

bool initHandleSet(RunCtx *runCtx, HandleSet *set);
void freeHandleSet(RunCtx *runCtx, HandleSet *set);
void handleSetAdd(HandleSet *set, EloxHandle *handle);
void handleSetRemove(RunCtx *runCtx, EloxHandle *handle);
void markHandleSet(HandleSet *set);

#endif // ELOX_HANDLE_SET_H
