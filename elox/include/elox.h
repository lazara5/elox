// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_H
#define ELOX_ELOX_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#include <elox-config.h>
#include <elox-defines.h>

typedef enum {
	ELOX_INTERPRET_OK,
	ELOX_INTERPRET_COMPILE_ERROR,
	ELOX_INTERPRET_RUNTIME_ERROR
} EloxInterpretResult;

typedef struct VM EloxVM;
typedef struct VMEnv EloxVMEnv;
typedef struct VMCtx EloxVMCtx;

typedef struct ObjFiber EloxFiber;

typedef struct EloxRunCtx {
	EloxVM *vm;
	EloxVMEnv *vmEnv;
	EloxFiber *activeFiber;
} EloxRunCtx;

#ifdef ELOX_ENABLE_NAN_BOXING
	typedef uint64_t EloxValue;
#else
	typedef struct EloxValue EloxValue;
#endif // ELOX_ENABLE_NAN_BOXING

typedef struct EloxError EloxError;

typedef EloxValue (*RTErr)(EloxRunCtx *runCtx, EloxError *error, const char *format, ...) ELOX_PRINTF(3, 4);
typedef EloxValue (*OOMErr)(EloxRunCtx *runCtx, EloxError *error);
typedef void (*DiscardException)(EloxFiber *fiber, size_t saved);

EloxValue runtimeError(EloxRunCtx *runCtx, EloxError *error, const char *format, ...) ELOX_PRINTF(3, 4);
EloxValue oomError(EloxRunCtx *runCtx, EloxError *error);
void discardException(EloxFiber *fiber, size_t saved);

typedef struct EloxError {
	bool raised;
	RTErr rtErr;
	OOMErr oomErr;
	DiscardException discardException;
} EloxError;

#define ELOX_ERROR_INITIALIZER { false, runtimeError, oomError, discardException }

EloxValue msgRuntimeError(EloxRunCtx *runCtx, EloxError *error, const char *format, ...);
EloxValue msgOomError(EloxRunCtx *runCtx, EloxError *error);
void msgDiscardException(EloxFiber *fiber, size_t saved);

typedef struct {
	bool raised;
	RTErr rtErr;
	OOMErr oomErr;
	DiscardException discardException;
	char msg[256];
} EloxMsgError;

#define ELOX_ERROR_MSG_INITIALIZER { false, msgRuntimeError, msgOomError, msgDiscardException, { 0 }}

#define RAISE_GET_FUNC(tuple) RAISE_GET_FUNC_ tuple
#define RAISE_GET_FUNC_(FUNC, ...) FUNC

#define RAISE_GET_CTX(tuple) RAISE_GET_CTX_ tuple
#define RAISE_GET_CTX_(FUNC, CTX, ...) CTX

#define RAISE_GET_FMT(tuple) RAISE_GET_FMT_ tuple
#define RAISE_GET_FMT_(FUNC, CTX, FMT, ...) FMT

#define RAISE_GET_ARGS(tuple) RAISE_GET_ARGS_ tuple
#define RAISE_GET_ARGS_(FUNC, CTX, FMT, ...) __VA_ARGS__

// First, define a macro that counts arguments using a reverse sequence.
#define GET_RAISE_SUFFIX(...) RAISE_NARG_(__VA_ARGS__, RAISE_SUFFIX_LIST())
#define RAISE_NARG_(...) EXPAND(RAISE_ARG_N(__VA_ARGS__))
#define RAISE_ARG_N( \
		 _1,  _2,  _3,  _4,  _5,  _6,  _7,  _8,  _9,  _10, \
		 _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, N, ...) N
#define RAISE_SUFFIX_LIST() \
		 FMT,FMT,FMT,FMT,FMT,FMT,FMT,FMT,FMT,FMT, \
		 FMT,FMT,FMT,FMT,FMT,FMT,FMT,STR,BASIC,?,?

#define RAISE_SUFFIX(tuple) EXPAND(GET_RAISE_SUFFIX tuple)

#define RAISE(ERR, CONSTR) RAISE_(RAISE_SUFFIX(CONSTR), ERR, CONSTR)
#define RAISE_(S, ERR, CONSTR) EXPAND(JOIN_(RAISE_, S)(ERR, CONSTR))

#define RAISE_FMT(ERR, CONSTR) \
	(ERR)->RAISE_GET_FUNC(CONSTR)(RAISE_GET_CTX(CONSTR), ERR, RAISE_GET_FMT(CONSTR), RAISE_GET_ARGS(CONSTR))
#define RAISE_STR(ERR, CONSTR) \
	(ERR)->RAISE_GET_FUNC(CONSTR)(RAISE_GET_CTX(CONSTR), ERR, RAISE_GET_FMT(CONSTR))
#define RAISE_BASIC(ERR, CONSTR) \
	(ERR)->RAISE_GET_FUNC(CONSTR)(RAISE_GET_CTX(CONSTR), ERR)

#define RTERR(RUNCTX, FMT, ...) \
	(rtErr, RUNCTX, "" FMT "", ## __VA_ARGS__)

#define OOM(RUNCTX) \
	(oomErr, RUNCTX)

#define ELOX_RAISE(ERROR, CONSTR) \
{ \
	if (!(ERROR)->raised) { \
		RAISE((ERROR), CONSTR); \
		(ERROR)->raised = true; \
	} \
}

#define ELOX_RAISE_RET(ERROR, ERRCONSTR) \
{ \
	ELOX_RAISE(ERROR, ERRCONSTR) \
	return; \
}

#define ELOX_RAISE_RET_VAL(ERROR, ERRCONSTR, val) \
{ \
	ELOX_RAISE(ERROR, ERRCONSTR) \
	return (val); \
}

#define ELOX_RAISE_GOTO(ERROR, ERRCONSTR, label) \
{ \
	ELOX_RAISE(ERROR, ERRCONSTR) \
	goto label; \
}

#define ELOX_CHECK_RAISE_RET(COND, ERROR, ERRCONSTR) \
{ \
	if (ELOX_UNLIKELY(!(COND))) { \
		ELOX_RAISE(ERROR, ERRCONSTR) \
		return; \
	} \
}

#define ELOX_CHECK_RET_VAL(COND, VAL) \
{ \
	if (ELOX_UNLIKELY(!(COND))) \
		return (VAL); \
}

#define ELOX_CHECK_RAISE_RET_VAL(cond, ERROR, ERRCONSTR, val) \
{ \
	if (ELOX_UNLIKELY(!(cond))) { \
		ELOX_RAISE(ERROR, ERRCONSTR) \
		return (val); \
	} \
}

#define ELOX_CHECK_GOTO(COND, LABEL) \
{ \
	if (ELOX_UNLIKELY(!(COND))) \
		goto LABEL; \
}

#define ELOX_CHECK_RAISE_GOTO(cond, ERROR, ERRCONSTR, label) \
{ \
	if (ELOX_UNLIKELY(!(cond))) { \
		ELOX_RAISE(ERROR, ERRCONSTR) \
		goto label; \
	} \
}

#define ELOX_IF_RAISED_RET_VAL(error, val) \
{ \
	if (ELOX_UNLIKELY((error)->raised)) \
		return (val); \
}

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
void eloxReleaseFiber(EloxRunCtx *runCtx, EloxHandle *fiber);

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
