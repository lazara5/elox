#ifndef SLOX_COMPILER_H
#define SLOX_COMPILER_H

#include "object.h"
#include "vm.h"

ObjFunction *compile(const char *source);
void markCompilerRoots();

#endif // SLOX_COMPILER_H
