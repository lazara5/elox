// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_H
#define ELOX_ELOX_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#include <elox-config.h>

typedef enum {
	ELOX_INTERPRET_OK,
	ELOX_INTERPRET_COMPILE_ERROR,
	ELOX_INTERPRET_RUNTIME_ERROR
} EloxInterpretResult;

typedef struct VM EloxVM;
typedef struct VMEnv EloxVMEnv;
typedef struct VMCtx EloxVMCtx;

typedef struct FiberCtx EloxFiberCtx;

typedef struct EloxRunCtx {
	EloxVM *vm;
	EloxVMEnv *vmEnv;
	EloxFiberCtx *activeFiber;
} EloxRunCtx;

typedef struct {
	const char *msg;
	bool raised;
} EloxError;

#define ELOX_ERROR_INITIALIZER { 0 }

#define ___ELOX_PUSH_RAISE(ERROR, PUSHERR) \
	if (!(ERROR)->raised) { \
		PUSHERR; \
		(ERROR)->raised = true; \
	}

#define ___PUSHERR(RUNCTX, func, ...) \
	func(RUNCTX, ## __VA_ARGS__)

#define RTERR(RUNCTX, fmt, ...) \
	___PUSHERR(RUNCTX, runtimeError, fmt, ## __VA_ARGS__)

#define OOM(RUNCTX) \
	___PUSHERR(RUNCTX, oomError)

#define ELOX_THROW_RET(ERROR, ERRCONSTR) \
{ \
	___ELOX_PUSH_RAISE(ERROR, ERRCONSTR) \
	return; \
}

#define ELOX_THROW_RET_VAL(ERROR, ERRCONSTR, val) \
{ \
	___ELOX_PUSH_RAISE(ERROR, ERRCONSTR) \
	return (val); \
}

#define ELOX_CHECK_THROW_RET(cond, ERROR, ERRCONSTR) \
{ \
	if (ELOX_UNLIKELY(!(cond))) { \
		___ELOX_PUSH_RAISE(ERROR, ERRCONSTR) \
		return; \
	} \
}

#define ELOX_CHECK_THROW_RET_VAL(cond, ERROR, ERRCONSTR, val) \
{ \
	if (ELOX_UNLIKELY(!(cond))) { \
		___ELOX_PUSH_RAISE(ERROR, ERRCONSTR) \
		return (val); \
	} \
}

#define ELOX_CHECK_THROW_GOTO(cond, ERROR, ERRCONSTR, label) \
{ \
	if (ELOX_UNLIKELY(!(cond))) { \
		___ELOX_PUSH_RAISE(ERROR, ERRCONSTR) \
		goto label; \
	} \
}

#define ELOX_RAISE(ERROR, MSG) { \
	if (!(ERROR)->raised) { \
		(ERROR)->msg = "" MSG ""; \
		(ERROR)->raised = true; \
	} \
}

#define ELOX_RAISE_STRMSG(ERROR, MSG) { \
	if (!(ERROR)->raised) { \
		(ERROR)->msg = (MSG); \
		(ERROR)->raised = true; \
	} \
}

#define ELOX_RAISE_RET_VAL(ERROR, MSG, val) { \
	if (!(ERROR)->raised) { \
		(ERROR)->msg = "" MSG ""; \
		(ERROR)->raised = true; \
	} \
	return (val); \
}

#define ELOX_CHECK_RAISE_GOTO(cond, ERROR, MSG, label) \
{ \
	if (ELOX_UNLIKELY(!(cond))) { \
		if (!(ERROR)->raised) { \
			(ERROR)->msg = "" MSG ""; \
			(ERROR)->raised = true; \
		} \
		goto label; \
	} \
}

#define ELOX_IF_RAISED_RET_VAL(error, val) \
{ \
	if (ELOX_UNLIKELY((error)->raised)) \
		return (val); \
}

#ifdef ELOX_ENABLE_NAN_BOXING
	typedef uint64_t EloxValue;
#else
	typedef struct EloxValue EloxValue;
#endif // ELOX_ENABLE_NAN_BOXING

typedef struct {
	const uint8_t *chars;
	int32_t length;
} EloxString;

typedef void *(*EloxRealloc)(void *oldPtr, size_t newSize, void *userData);
typedef void (*EloxFree)(void *ptr, void *userData);

typedef struct {
	EloxRealloc realloc;
	EloxFree free;
	void *userData;
} EloxAllocator;

typedef enum {
	ELOX_IO_OUT,
	ELOX_IO_ERR,
	ELOX_IO_DEBUG
} EloxIOStream;

typedef void (*EloxIOWrite)(EloxIOStream stream, const char *data, uint32_t len);

typedef EloxValue (*ModuleLoader)(EloxRunCtx *runCtx, const EloxString *moduleName, uint64_t options,
								  EloxError *error);

typedef struct {
	ModuleLoader loader;
	uint64_t options;
} EloxModuleLoader;

typedef enum {
	ELOX_BML_ENABLE_SYS = 1 << 0,
	ELOX_BML_ENABLE_ALL = ELOX_BML_ENABLE_SYS
} EloxBuiltinModuleLoaderOptions;

EloxValue eloxBuiltinModuleLoader(EloxRunCtx *runCtx, const EloxString *moduleName, uint64_t options,
								  EloxError *error);

EloxValue eloxFileModuleLoader(EloxRunCtx *runCtx, const EloxString *moduleName, uint64_t options,
							   EloxError *error);

EloxValue eloxNativeModuleLoader(EloxRunCtx *runCtx, const EloxString *moduleName, uint64_t options,
								 EloxError *error);

typedef struct EloxConfig {
	EloxAllocator allocator;
	EloxIOWrite writeCallback;
	EloxModuleLoader *moduleLoaders;
} EloxConfig;

void eloxInitConfig(EloxConfig *config);
EloxVMCtx *eloxNewVMCtx(const EloxConfig *config);
void eloxDestroyVMCtx(EloxVMCtx *vmCtx);

typedef struct EloxHandle EloxHandle;
typedef struct EloxRunCtxHandle EloxRunCtxHandle;

void eloxReleaseHandle(EloxHandle *handle);

EloxRunCtxHandle *eloxNewRunCtx(EloxVMCtx *vmCtx);
void eloxReleaseFiberCtx(EloxRunCtx *runCtx, EloxHandle *fiber);

typedef struct EloxCallableHandle EloxCallableHandle;

static const char *eloxMainModuleName = "<main>";

EloxCallableHandle *eloxGetFunction(EloxRunCtxHandle *runHandle, const char *name, const char *module);

typedef struct {
	EloxRunCtx *runCtx;
	uint16_t numArgs;
	uint16_t maxArgs;
} EloxCallableInfo;

EloxCallableInfo eloxPrepareCall(EloxCallableHandle *callableHandle);

EloxInterpretResult eloxCall(const EloxCallableInfo *callableInfo);

void eloxPushDouble(EloxCallableInfo *callableInfo, double val);

double eloxGetResultDouble(EloxCallableInfo *callableInfo);

const char *eloxGetResultString(EloxCallableInfo *callableInfo);

#endif // ELOX_ELOX_H
