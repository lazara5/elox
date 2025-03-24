// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_OBJTAGS_INLINE
#define INITTAG(name, init)
#define TAG(name)
#endif // ELOX_OBJTAGS_INLINE

INITTAG(STRING, VAL_OBJ)
TAG(BOUND_METHOD)
TAG(METHOD)
TAG(DEFAULT_METHOD)
TAG(METHOD_DESC)
TAG(INTERFACE)
TAG(CLASS)
TAG(CLOSURE)
TAG(NATIVE_CLOSURE)
TAG(FUNCTION)
TAG(INSTANCE)
TAG(NATIVE)
TAG(STRINGPAIR)
TAG(UPVALUE)
TAG(ARRAY)
TAG(TUPLE)
TAG(HASHMAP)
