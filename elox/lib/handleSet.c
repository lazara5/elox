// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/elox-internal.h"
#include "elox/value.h"
#include "elox/handleSet.h"

#include <stdlib.h>

void initHandleSet(VMCtx *vmCtx, HandleSet *set) {
	set->head = ALLOCATE(vmCtx, EloxHandle, 1);
	set->head->next = set->head->prev = set->head;
}

static void freeHandle(VMCtx *vmCtx, EloxHandle *handle) {
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];
	GENERIC_FREE(vmCtx, desc->handleSize, handle);
}

void freeHandleSet(VMCtx *vmCtx, HandleSet *set) {
	EloxHandle *current = set->head->next;
	EloxHandle *next;

	while (current != set->head) {
		next = current->next;
		freeHandle(vmCtx, current);
		current = next;
	}

	FREE(vmCtx, EloxHandle, set->head);
	set->head = NULL;
}

void handleSetAdd(HandleSet *set, EloxHandle *handle) {
	EloxHandle *head = set->head;
	handle->next = head->next;
	handle->prev = head;
	head->next->prev = handle;
	head->next = handle;
}

void handleSetRemove(VMCtx *vmCtx, EloxHandle *handle) {
	if (handle == NULL)
		return;

	handle->next->prev = handle->prev;
	handle->prev->next = handle->next;

	freeHandle(vmCtx, handle);
}

void markHandleSet(VMCtx *vmCtx, HandleSet *set) {
	if (ELOX_UNLIKELY(set->head == NULL))
		return;

	EloxHandle *handle = set->head->next;
	while (handle != set->head) {
		markHandle(vmCtx, handle);
		handle = handle->next;
	}
}
