// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifdef ELOX_ENABLE_COMPUTED_GOTO

#define OBJ_TAG_DISPATCH_START(value)   goto *objTagDispatchTable[value - VAL_OBJ];
#define OBJ_TAG_DISPATCH_CASE(name)     JOIN(objTagDispatch_, name)
#define OBJ_TAG_DISPATCH_BREAK          goto objTagDispatchEnd
#define OBJ_TAG_DISPATCH_END            objTagDispatchEnd: ;

#else

#define OBJ_TAG_DISPATCH_START(value)   switch(value) {
#define OBJ_TAG_DISPATCH_CASE(name)     case JOIN(OBJ_, name)
#define OBJ_TAG_DISPATCH_BREAK          break
#define OBJ_TAG_DISPATCH_END }

#endif // ELOX_ENABLE_COMPUTED_GOTO
