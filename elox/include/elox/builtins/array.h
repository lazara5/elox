#ifndef ELOX_BUILTINS_ARRAY_H
#define ELOX_BUILTINS_ARRAY_H

#include <elox/function.h>

Value arrayIteratorHasNext(Args *args);
Value arrayIteratorNext(Args *args);
Value arrayIteratorRemove(Args *args);
Value arrayIterator(Args *args);
Value arrayLength(Args *args);
Value arrayAdd(Args *args);
Value arrayRemoveAt(Args *args);

#endif
