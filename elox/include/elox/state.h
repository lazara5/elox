// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_STATE_H
#define ELOX_STATE_H

#include "elox/vm.h"
#include "elox/scanner.h"

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

typedef struct VMInst {
	VM instance;
	VMEnv env;
	VMCtx vmCtx;
} VMInst;

typedef EloxRunCtx RunCtx;

typedef struct CCtx {
	Scanner scanner;
	EloxCompilerHandle *compilerHandle;
	String moduleName;
	int moduleNameLength;
	RunCtx *runCtx;
} CCtx;

#endif
