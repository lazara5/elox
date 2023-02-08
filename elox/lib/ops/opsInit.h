// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef OPNAME
#error OPNAME not defined
#endif

#ifdef ELOX_ENABLE_COMPUTED_GOTO

#define OP_DISPATCH_START(value)   goto *JOIN(OPNAME, DispatchTable)[value];
#define OP_DISPATCH_CASE(name)     JOIN(JOIN(OPNAME, _opcode_), name)
#define OP_DISPATCH_BREAK          goto JOIN(OPNAME, dispatchEnd)
#define OP_DISPATCH_END            JOIN(OPNAME, dispatchEnd): ;

#else

#define OP_DISPATCH_START(value)   switch(value) {
#define OP_DISPATCH_CASE(name)     case JOIN(JOIN(OPNAME, _OP_), name)
#define OP_DISPATCH_BREAK          break
#define OP_DISPATCH_END }

#endif // ELOX_ENABLE_COMPUTED_GOTO
