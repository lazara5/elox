#ifndef ELOX_BUILTINS_H
#define ELOX_BUILTINS_H

#include "elox/state.h"

static String eloxBuiltinModule = STRING_INITIALIZER("<builtin>");

uint16_t builtinConstant(VMCtx *vmCtx, const String *name);

void registerBuiltins(VMCtx *vmCtx);

void clearBuiltins(VM *vm);

#endif // ELOX_BUILTINS_H
