#include "elox/memory.h"
#include "elox/object.h"
#include "elox/elox-internal.h"
#include "elox/value.h"
#include "elox/handleSet.h"

#include <stdlib.h>

void initHandleSet(HandleSet *set) {
	set->head = NULL;
}

static void freeHandle(VMCtx *vmCtx, EloxHandle *handle) {
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];
	GENERIC_FREE(vmCtx, desc->handleSize, handle);
}

void freeHandleSet(VMCtx *vmCtx, HandleSet *set) {
	EloxHandle *current = set->head;
	EloxHandle *next;

	while (current != NULL) {
		next = current->next;
		freeHandle(vmCtx, current);
		current = next;
	}

	set->head = NULL;
}

void handleSetAdd(HandleSet *set, EloxHandle *handle) {
	handle->next = set->head;
	handle->prev = NULL;

	if (set->head != NULL)
		set->head->prev = handle;

	set->head = handle;
}

void handleSetRemove(VMCtx *vmCtx, HandleSet *set, EloxHandle *handle) {
	if (set == NULL || handle == NULL)
		return;

	if (set->head == handle)
		set->head = handle->next;

	if (handle->next != NULL)
		handle->next->prev = handle->prev;

	if (handle->prev != NULL)
		handle->prev->next = handle->next;

	freeHandle(vmCtx, handle);
}

void markHandleSet(VMCtx *vmCtx, HandleSet *set) {
	EloxHandle *handle = set->head;
	while (handle != NULL) {
		markHandle(vmCtx, handle);
		handle = handle->next;
	}
}
