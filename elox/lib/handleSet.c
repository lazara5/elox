// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/elox-internal.h"
#include <elox/handleSet.h>

#include <stdlib.h>

bool initHandleSet(RunCtx *runCtx, HandleSet *set) {
	set->head = ALLOCATE(runCtx, EloxHandle, 1);
	if (ELOX_UNLIKELY(set->head == NULL))
		return false;
	set->head->next = set->head;
	return true;
}

static void freeHandle(VMCtx *vmCtx, EloxHandle *handle) {
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];
	GENERIC_FREE(vmCtx, desc->handleSize, handle);
}

void freeHandleSet(VMCtx *vmCtx, HandleSet *set) {
	if (set->head == NULL)
		return;

	EloxHandle *current = set->head->next;

	while (current != set->head) {
		EloxHandle *next = current->next;
		freeHandle(vmCtx, current);
		current = next;
	}

	FREE(vmCtx, EloxHandle, set->head);
	set->head = NULL;
}

void handleSetAdd(HandleSet *set, EloxHandle *handle) {
	EloxHandle *head = set->head;
	handle->next = head->next;
	head->next = handle;
	handle->live = true;
}

void handleSetRemove(EloxHandle *handle) {
	if (handle == NULL)
		return;

	handle->live = false;
}

static void markHandle(EloxHandle *handle) {
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];
	desc->mark(handle);
}

void markHandleSet(VMCtx *vmCtx, HandleSet *set) {
	if (ELOX_UNLIKELY(set->head == NULL))
		return;

	EloxHandle *prev = set->head;
	EloxHandle *handle = set->head->next;
	while (handle != set->head) {
		if (handle->live) {
			markHandle(handle);
			prev = handle;
			handle = handle->next;
		} else {
			prev->next = handle->next;
			EloxHandle *inactiveHandle = handle;
			handle = handle->next;
			freeHandle(vmCtx, inactiveHandle);
		}
	}
}
