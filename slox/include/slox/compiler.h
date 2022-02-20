#ifndef SLOX_COMPILER_H
#define SLOX_COMPILER_H

#include "slox/object.h"
#include "slox/vm.h"

ObjFunction *compile(VMCtx *vmCtx, const char *source);
void markCompilerRoots(VMCtx *vmCtx);

#endif // SLOX_COMPILER_H
