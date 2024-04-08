// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_STATE_H
#define ELOX_STATE_H

#include "elox/vm.h"
#include "elox/memory.h"
#include "elox/scanner.h"
#include "elox/compiler.h"

#ifndef NDEBUG
#include <assert.h>
#endif

typedef struct VMEnv {
	EloxRealloc realloc;
	EloxFree free;
	void *allocatorUserData;

	EloxIOWrite write;
	EloxModuleLoader *loaders;
} VMEnv;

typedef struct VMCtx {
	VM vmInstance;
	VMEnv env;
} VMCtx;

typedef EloxRunCtx RunCtx;

typedef struct CCtx {
	Scanner scanner;
	CompilerState compilerState;
	String moduleName;
	int moduleNameLength;
	RunCtx *runCtx;
} CCtx;

typedef struct {
	VMTemp *oldTemps;
	VMTemp **head;
} TmpScope;

#define TMP_SCOPE_INITIALIZER(fiber) { .head = &((fiber)->temps), .oldTemps = (fiber)->temps }

#ifdef NDEBUG
	#define TEMP_INITIALIZER { .next = NULL }
	#define TEMP_INITIALIZER_VAL(value) { .val = value }
#else
	#define TEMP_INITIALIZER { .pushed = false }
	#define TEMP_INITIALIZER_VAL(value) { .val = value, .pushed = false }
#endif

static inline void pushTemp(TmpScope temps, VMTemp *temp) {
#ifndef NDEBUG
	assert(temp->pushed == false);
	temp->pushed = true;
#endif
	temp->next = *temps.head;
	*temps.head = temp;
}

#define PUSH_TEMP(SCOPE, NAME, VALUE) \
	VMTemp NAME = TEMP_INITIALIZER_VAL(VALUE); \
	pushTemp(SCOPE, &NAME);

static inline void pushTempVal(TmpScope temps, VMTemp *temp, Value val) {
	temp->val = val;
	temp->next = *temps.head;
	*temps.head = temp;
}

static inline void releaseTemps(TmpScope *temps) {
	*temps->head = temps->oldTemps;
}

#endif
