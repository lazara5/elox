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
	uint16_t discardArgs;
} EloxCallableInfo;

#ifdef ELOX_ENABLE_NAN_BOXING
	typedef uint64_t EloxValue;
#else
	typedef struct EloxValue EloxValue;
#endif // ELOX_ENABLE_NAN_BOXING

EloxCallableInfo eloxPrepareCall(EloxVM *vmCtx, EloxCallableHandle *handle, int16_t numArgs);

EloxInterpretResult eloxCall(EloxVM *vmCtx, const EloxCallableInfo *callableInfo);

void eloxSetSlotDouble(EloxCallableInfo *callableInfo, uint16_t slot, double val);

double eloxGetResultDouble(EloxCallableInfo *callableInfo);

const char *eloxGetResultString(EloxCallableInfo *callableInfo);

#endif // ELOX_ELOX_H
