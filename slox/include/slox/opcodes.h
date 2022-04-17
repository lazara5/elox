#ifndef SLOX_OPCODES_INLINE
typedef enum {
#define OPCODE(name) OP_##name,
#endif

OPCODE(CONST8)
OPCODE(CONST16)
OPCODE(IMM8)
OPCODE(IMM16)
OPCODE(NIL)
OPCODE(TRUE)
OPCODE(FALSE)
OPCODE(POP)
OPCODE(POPN)
OPCODE(GET_LOCAL)
OPCODE(GET_GLOBAL)
OPCODE(DEFINE_GLOBAL)
OPCODE(SET_LOCAL)
OPCODE(SET_GLOBAL)
OPCODE(GET_UPVALUE)
OPCODE(SET_UPVALUE)
OPCODE(GET_PROPERTY)
OPCODE(GET_MEMBER_PROPERTY)
OPCODE(SET_PROPERTY)
OPCODE(SET_MEMBER_PROPERTY)
OPCODE(GET_SUPER)
OPCODE(EQUAL)
OPCODE(GREATER)
OPCODE(LESS)
OPCODE(ADD)
OPCODE(SUBTRACT)
OPCODE(MULTIPLY)
OPCODE(DIVIDE)
OPCODE(MODULO)
OPCODE(NOT)
OPCODE(NEGATE)
OPCODE(JUMP)
OPCODE(JUMP_IF_FALSE)
OPCODE(LOOP)
OPCODE(CALL)
OPCODE(INVOKE)
OPCODE(MEMBER_INVOKE)
OPCODE(SUPER_INVOKE)
OPCODE(SUPER_INIT)
OPCODE(CLOSURE)
OPCODE(CLOSE_UPVALUE)
OPCODE(RETURN)
OPCODE(CLASS)
OPCODE(INHERIT)
OPCODE(METHOD)
OPCODE(FIELD)
OPCODE(RESOLVE_MEMBERS)
OPCODE(ARRAY_BUILD)
OPCODE(INDEX)
OPCODE(INDEX_STORE)
OPCODE(MAP_BUILD)
OPCODE(THROW)
OPCODE(PUSH_EXCEPTION_HANDLER)
OPCODE(POP_EXCEPTION_HANDLER)
OPCODE(FOREACH_INIT)
OPCODE(UNPACK)
OPCODE(DATA)

#ifndef SLOX_OPCODES_INLINE
#undef OPCODE
} OpCode;
#endif
