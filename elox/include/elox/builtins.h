#ifndef ELOX_BUILTINS_H
#define ELOX_BUILTINS_H

#include "elox/state.h"

void registerBuiltins(VMCtx *vmCtx);

void markBuiltins(VMCtx *vmCtx);

void clearBuiltins(VM *vm);

#endif // ELOX_BUILTINS_H
