#ifndef ELOX_BUILTINS_H
#define ELOX_BUILTINS_H

#include "elox/state.h"

static String eloxBuiltinModule = STRING_INITIALIZER("<builtin>");

suint16_t builtinConstant(VMCtx *vmCtx, const String *name);

bool registerBuiltins(VMCtx *vmCtx);

void clearBuiltins(VM *vm);

#endif // ELOX_BUILTINS_H
