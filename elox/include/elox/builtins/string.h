#ifndef ELOX_BUILTINS_STRING_H
#define ELOX_BUILTINS_STRING_H

#include "elox/state.h"

Value stringFmt(Args *args);
Value printFmt(Args *args);
Value stringMatch(Args *args);
Value stringGsub(Args *args);
Value gmatchIteratorHasNext(Args *args);
Value gmatchIteratorNext(Args *args);
Value stringGmatch(Args *args);
Value stringStartsWith(Args *args);
Value stringEndsWith(Args *args);
Value stringUpper(Args *args);
Value stringLower(Args *args);
Value stringAtSafe(VMCtx *vmCtx, ObjString *str, int32_t index);

#endif
