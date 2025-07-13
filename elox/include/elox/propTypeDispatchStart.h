// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifdef ELOX_ENABLE_COMPUTED_GOTO

#define PROP_TYPE_DISPATCH_START(value)   goto *propTypeDispatchTable[value];
#define PROP_TYPE_DISPATCH_CASE(name)     JOIN(propTypeDispatch_, name)
#define PROP_TYPE_DISPATCH_BREAK          goto propTypeDispatchEnd
#define PROP_TYPE_DISPATCH_END            propTypeDispatchEnd: ;

#else

#define PROP_TYPE_DISPATCH_START(value)   switch(value) {
#define PROP_TYPE_DISPATCH_CASE(name)     case JOIN(ELOX_PROP_, name)
#define PROP_TYPE_DISPATCH_BREAK          break
#define PROP_TYPE_DISPATCH_END }

#endif // ELOX_ENABLE_COMPUTED_GOTO
