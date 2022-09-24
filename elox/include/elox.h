#ifndef ELOX_ELOX_H
#define ELOX_ELOX_H

#include "elox-config.h"

#include <stdint.h>

typedef enum {
	ELOX_INTERPRET_OK,
	ELOX_INTERPRET_COMPILE_ERROR,
	ELOX_INTERPRET_RUNTIME_ERROR
} EloxInterpretResult;

typedef struct VMCtx VMCtx;

typedef struct EloxHandle EloxHandle;
typedef struct EloxCallableHandle EloxCallableHandle;

static const char *eloxMainModuleName = "<main>";
static const char *eloxBuiltinModuleName = "<builtin>";

EloxCallableHandle *eloxGetFunction(VMCtx *vmCtx, const char *name, const char *module);

typedef struct {
	VMCtx *vmCtx;
	uint16_t numArgs;
	uint16_t discardArgs;
} EloxCallableInfo;

#ifdef ELOX_ENABLE_NAN_BOXING
	typedef uint64_t EloxValue;
#else
	typedef struct EloxValue EloxValue;
#endif // ELOX_ENABLE_NAN_BOXING

EloxCallableInfo eloxPrepareCall(VMCtx *vmCtx, EloxCallableHandle *handle, int16_t numArgs);

EloxInterpretResult eloxCall(VMCtx *vmCtx, const EloxCallableInfo *callableInfo);

void EloxSetSlotDouble(EloxCallableInfo *callableInfo, uint16_t slot, double val);

#endif // ELOX_ELOX_H
