#ifndef SLOX_BUILTINS_H
#define SLOX_BUILTINS_H

#include "slox/state.h"

void registerBuiltins(VMCtx *vmCtx);

void markBuiltins(VMCtx *vmCtx);

void clearBuiltins(VM *vm);

#endif // SLOX_BUILTINS_H
