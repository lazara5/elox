#ifndef ELOX_BUILTINS_H
#define ELOX_BUILTINS_H

#include "elox/state.h"

static String eloxBuiltinModule = STRING_INITIALIZER("<builtin>");

void registerBuiltins(VMCtx *vmCtx);

void markBuiltins(VMCtx *vmCtx);

void clearBuiltins(VM *vm);

//--- String ----------------------

Value stringFmt(Args *args);
Value stringMatch(Args *args);
Value stringGsub(Args *args);
Value stringStartsWith(Args *args);
Value stringEndsWith(Args *args);

#endif // ELOX_BUILTINS_H
