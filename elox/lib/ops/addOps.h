// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_OPCODES_INLINE
typedef enum {
#define OPCODE(name) ADD_OP_##name,
#endif // ELOX_OPCODES_INLINE

OPCODE(UNDEFINED)
OPCODE(NUMBER_NUMBER)
OPCODE(STRING_STRING)

#ifndef ELOX_OPCODES_INLINE
#undef OPCODE
} OpCode;
#endif // ELOX_OPCODES_INLINE
