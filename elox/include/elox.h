// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_H
#define ELOX_ELOX_H

#include <stdint.h>
#include <stddef.h>

#include <elox-config.h>

typedef enum {
	ELOX_INTERPRET_OK,
	ELOX_INTERPRET_COMPILE_ERROR,
	ELOX_INTERPRET_RUNTIME_ERROR
} EloxInterpretResult;

typedef struct VMCtx EloxVM;

typedef enum {
	ELOX_IO_OUT,
	ELOX_IO_ERR,
	ELOX_IO_DEBUG
} EloxIOStream;

typedef void (*EloxIOWrite)(EloxIOStream stream, const char *data, uint32_t len);

typedef struct EloxConfig {
	EloxIOWrite writeCallback;
} EloxConfig;

void eloxInitConfig(EloxConfig *config);

typedef struct EloxHandle EloxHandle;
typedef struct EloxCallableHandle EloxCallableHandle;

static const char *eloxMainModuleName = "<main>";
static const char *eloxBuiltinModuleName = "<builtin>";

EloxCallableHandle *eloxGetFunction(EloxVM *vmCtx, const char *name, const char *module);

typedef struct {
	EloxVM *vmCtx;
	uint16_t numArgs;
	uint16_t maxArgs;
} EloxCallableInfo;

#ifdef ELOX_ENABLE_NAN_BOXING
	typedef uint64_t EloxValue;
#else
	typedef struct EloxValue EloxValue;
#endif // ELOX_ENABLE_NAN_BOXING

EloxCallableInfo eloxPrepareCall(EloxVM *vmCtx, EloxCallableHandle *handle);

EloxInterpretResult eloxCall(EloxVM *vmCtx, const EloxCallableInfo *callableInfo);

void eloxPushDouble(EloxCallableInfo *callableInfo, double val);

double eloxGetResultDouble(EloxCallableInfo *callableInfo);

const char *eloxGetResultString(EloxCallableInfo *callableInfo);

#endif // ELOX_ELOX_H
