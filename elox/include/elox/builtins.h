#ifndef ELOX_BUILTINS_H
#define ELOX_BUILTINS_H

#include "elox/state.h"

static String eloxBuiltinModule = STRING_INITIALIZER("<builtin>");

void registerBuiltins(VMCtx *vmCtx);

void markBuiltins(VMCtx *vmCtx);

void clearBuiltins(VM *vm);

Value stringFmt(VMCtx *vmCtx, int argCount, Value *args);

#endif // ELOX_BUILTINS_H
