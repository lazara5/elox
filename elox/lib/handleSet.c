// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/elox-internal.h"
#include "elox/value.h"
#include "elox/handleSet.h"

#include <stdlib.h>

bool initHandleSet(RunCtx *runCtx, HandleSet *set) {
	set->head = ALLOCATE(runCtx, EloxHandle, 1);
	if (ELOX_UNLIKELY(set->head == NULL))
		return false;
	set->head->next = set->head->prev = set->head;
	return true;
}

static void freeHandle(RunCtx *runCtx, EloxHandle *handle) {
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];
	GENERIC_FREE(runCtx, desc->handleSize, handle);
}

void freeHandleSet(RunCtx *runCtx, HandleSet *set) {
	if (set->head == NULL)
		return;

	EloxHandle *current = set->head->next;

	while (current != set->head) {
		EloxHandle *next = current->next;
		freeHandle(runCtx, current);
		current = next;
	}

	FREE(runCtx, EloxHandle, set->head);
	set->head = NULL;
}

void handleSetAdd(HandleSet *set, EloxHandle *handle) {
	EloxHandle *head = set->head;
	handle->next = head->next;
	handle->prev = head;
	head->next->prev = handle;
	head->next = handle;
}

void handleSetRemove(RunCtx *runCtx, EloxHandle *handle) {
	if (handle == NULL)
		return;

	handle->next->prev = handle->prev;
	handle->prev->next = handle->next;

	freeHandle(runCtx, handle);
}

static void markHandle(EloxHandle *handle) {
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];
	desc->mark(handle);
}

void markHandleSet(HandleSet *set) {
	if (ELOX_UNLIKELY(set->head == NULL))
		return;

	EloxHandle *handle = set->head->next;
	while (handle != set->head) {
		markHandle(handle);
		handle = handle->next;
	}
}
