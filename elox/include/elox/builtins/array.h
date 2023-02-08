// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
