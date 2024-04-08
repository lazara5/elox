#ifndef ELOX_BUILTINS_H
#define ELOX_BUILTINS_H

#include "elox/state.h"

static String eloxBuiltinModule = ELOX_STRING("<builtin>");

suint16_t builtinConstant(RunCtx *runCtx, const String *name);

bool registerBuiltins(RunCtx *runCtx);

void clearBuiltins(VM *vm);

#endif // ELOX_BUILTINS_H
