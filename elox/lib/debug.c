// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/debug.h"
#include "elox/object.h"
#include "elox/compiler.h"
#include <elox/state.h>
#include <elox/vm.h>

#include <string.h>

void disassembleChunk(RunCtx *runCtx, Chunk *chunk, const char *name) {
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "== %s ==\n", name);

	for (int offset = 0; offset < chunk->count; )
		offset = disassembleInstruction(runCtx, chunk, offset);

	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "== END CHUNK ==\n");
}

static int constantByteInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant = chunk->code[offset + 1];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 2;
}

static int constantUShortInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 3;
}

static int globalInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, constant);
	return offset + 3;
}

static int builtinInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, constant);
	return offset + 3;
}

static int getPropertyInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 3;
}

static int invokeInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t argCount = chunk->code[offset + 3];
	uint8_t hasExpansions = chunk->code[offset + 4];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s (%d args %d) %4d (", name, argCount, hasExpansions, constant);
	printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 5;
}

static int memberInvokeInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t slot;
	memcpy(&slot, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t argCount = chunk->code[offset + 3];
	uint8_t hasExpansions = chunk->code[offset + 4];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s (%d args %d) %u\n", name, argCount, hasExpansions, slot);
	return offset + 5;
}

static int simpleInstruction(RunCtx *runCtx, const char *name, int offset) {
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%s\n", name);
	return offset + 1;
}

static int byteInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t val = chunk->code[offset + 1];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %4d\n", name, val);
	return offset + 2;
}

static int shortInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t val;
	memcpy(&val, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, val);
	return offset + 3;
}

static int intInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	int32_t val;
	memcpy(&val, &chunk->code[offset + 1], sizeof(int32_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, val);
	return offset + 5;
}

static int jumpInstruction(RunCtx *runCtx, const char *name, int sign, Chunk *chunk, int offset) {
	uint16_t jump;
	memcpy(&jump, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
	return offset + 3;
}

static int callInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t numArgs = chunk->code[offset + 1];
	uint8_t hasExpansions = chunk->code[offset + 2];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %4d %4d\n", name, numArgs, hasExpansions);
	return offset + 3;
}

static int exceptionHandlerInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t stackLevel = chunk->code[offset + 1];
	uint16_t handlerData;
	memcpy(&handlerData, &chunk->code[offset + 2], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s [%d] @%d\n", name, stackLevel, handlerData);
	return offset + 4;
}

static int arrayBuildInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t objType = chunk->code[offset + 1];
	uint16_t numItems;
	memcpy(&numItems, &chunk->code[offset + 2], sizeof(uint16_t));
	const char *type = (objType == OBJ_ARRAY) ? "ARRAY" : "TUPLE";
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %s [%d]\n", name, type, numItems);
	return offset + 4;
}

static int forEachInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t hasNextSlot = chunk->code[offset + 1];
	bool hasNextPostArgs = chunk->code[offset + 2];
	uint8_t nextSlot = chunk->code[offset + 3];
	bool nextPostArgs = chunk->code[offset + 4];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %4d %4d\n", name, hasNextSlot, nextSlot);
	return offset + 5;
}

static int unpackInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t numVal = chunk->code[offset + 1];
	bool first = true;
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s ", name);
	int argOffset = offset + 2;
	int argSize = 0;
	for (int i = 0; i < numVal; i++) {
		if (!first)
			ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ", ");
		first = false;

		VarType varType = chunk->code[argOffset];
		switch (varType) {
			case VAR_LOCAL:
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "L %d %s",
							chunk->code[argOffset + 1], chunk->code[argOffset + 1] ? "POST" : "PRE");
				argSize += 3;
				argOffset += 3;
				break;
			case VAR_UPVALUE:
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "U %d", chunk->code[argOffset + 1]);
				argSize += 2;
				argOffset += 2;
				break;
			case VAR_GLOBAL: {
				uint16_t slot;
				memcpy(&slot, &chunk->code[argOffset + 1], sizeof(uint16_t));
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "G %d", slot);
				argSize += 3;
				argOffset += 3;
				break;
			}
		}
	}
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");
	return offset + 1 + 1 + argSize;
}

static int resolveMembersInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t numMembers;
	memcpy(&numMembers, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s\n", name);

	for (int i = 0; i < numMembers; i++) {
		unsigned char *entry = chunk->code + offset + 3 + 5 * i;
		uint8_t type = entry[0];
		bool super = type & 0x1;
		uint8_t mask = (type & 0x6) >> 1;
		const char *strMask[] = {"field", "method", "any"};
		uint16_t nameIndex;
		memcpy(&nameIndex, &entry[1], sizeof(uint16_t));
		uint16_t slot;
		memcpy(&slot, &entry[3], sizeof(uint16_t));
		eloxPrintf(runCtx, ELOX_IO_DEBUG,
				   "        |                        [%u]=%s[%s %u (",
				   slot, super ? "super" : "this",
				   strMask[mask - 1], nameIndex);
		printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[nameIndex]);
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")]\n");
	}

	return offset + 3 + 5 * numMembers;
}

static int dataInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t len = chunk->code[offset + 1];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s [%d]=[", name, len);
	for (int i = 0; i < len; i++) {
		if (i == 0)
			eloxPrintf(runCtx, ELOX_IO_DEBUG, "%d", chunk->code[offset + 2 + i]);
		else
			eloxPrintf(runCtx, ELOX_IO_DEBUG, ",%d", chunk->code[offset + 2 + i]);
	}
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "]\n");
	return offset + 1 + len + 1;
}

static int localInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	uint8_t postArgs = chunk->code[offset + 2];
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %5d %s\n", name, slot, postArgs ? "POST" : "PRE");
	return offset + 3;
}

static int importInstruction(RunCtx *runCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t module;
	memcpy(&module, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s ", name);
	printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[module]);
	uint16_t numArgs;
	memcpy(&numArgs, &chunk->code[offset + 3], sizeof(uint16_t));
	eloxPrintf(runCtx, ELOX_IO_DEBUG, " %u (", numArgs);
	for (int i = 0; i < numArgs; i++) {
		uint16_t sym;
		if (i > 0)
			ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ", ");
		memcpy(&sym, &chunk->code[offset + 5 + 2 * i], sizeof(uint16_t));
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%u", sym);
	}
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")");

	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");
	return offset + 5 + 2 * numArgs;
}

int disassembleInstruction(RunCtx *runCtx, Chunk *chunk, int offset) {
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%04d ", offset);
	int line = getLine(chunk, offset);
	if (offset > 0 && line == getLine(chunk, offset - 1)) {
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "   | ");
	} else {
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%4d ", line);
	}

	uint8_t instruction = chunk->code[offset];
	switch (instruction) {
		case OP_CONST8:
			return constantByteInstruction(runCtx, "CONST8", chunk, offset);
		case OP_CONST16:
			return constantUShortInstruction(runCtx, "CONST16", chunk, offset);
		case OP_IMMI:
			return intInstruction(runCtx, "IMMI", chunk, offset);
		case OP_NIL:
			return simpleInstruction(runCtx, "NIL", offset);
		case OP_TRUE:
			return simpleInstruction(runCtx, "TRUE", offset);
		case OP_FALSE:
			return simpleInstruction(runCtx, "FALSE", offset);
		case OP_POP:
			return simpleInstruction(runCtx, "POP", offset);
		case OP_POPN:
			return byteInstruction(runCtx, "POPN", chunk, offset);
		case OP_SWAP:
			return simpleInstruction(runCtx, "SWAP", offset);
		case OP_NUM_VARARGS:
			return simpleInstruction(runCtx, "NUM_VARARGS", offset);
		case OP_EXPAND_VARARGS:
			return byteInstruction(runCtx, "EXPAND_VARARGS", chunk, offset);
		case OP_EXPAND:
			return byteInstruction(runCtx, "EXPAND", chunk, offset);
		case OP_PEEK:
			return byteInstruction(runCtx, "PEEK", chunk, offset);
		case OP_GET_LOCAL:
			 return localInstruction(runCtx, "GET_LOCAL", chunk, offset);
		case OP_GET_VARARG:
			return simpleInstruction(runCtx, "GET_VARARG", offset);
		case OP_SET_LOCAL:
			return localInstruction(runCtx, "SET_LOCAL", chunk, offset);
		case OP_SET_VARARG:
			return simpleInstruction(runCtx, "SET_VARARG", offset);
		case OP_GET_GLOBAL:
			return globalInstruction(runCtx, "GET_GLOBAL", chunk, offset);
		case OP_GET_BUILTIN:
			return builtinInstruction(runCtx, "GET_BUILTIN", chunk, offset);
		case OP_DEFINE_GLOBAL:
			return globalInstruction(runCtx, "DEFINE_GLOBAL", chunk, offset);
		case OP_SET_GLOBAL:
			return globalInstruction(runCtx, "SET_GLOBAL", chunk, offset);
		case OP_GET_UPVALUE:
			return byteInstruction(runCtx, "GET_UPVALUE", chunk, offset);
		case OP_SET_UPVALUE:
			return byteInstruction(runCtx, "SET_UPVALUE", chunk, offset);
		case OP_GET_PROP:
			return getPropertyInstruction(runCtx, "GET_PROP", chunk, offset);
		case OP_MAP_GET:
			return shortInstruction(runCtx, "MAP_GET", chunk, offset);
		case OP_GET_MEMBER_PROP:
			return shortInstruction(runCtx, "GET_MEMBER_PROP", chunk, offset);
		case OP_SET_PROP:
			return constantUShortInstruction(runCtx, "SET_PROP", chunk, offset);
		case OP_SET_MEMBER_PROP:
			return shortInstruction(runCtx, "SET_MEMBER_PROP", chunk, offset);
		case OP_MAP_SET:
			return constantUShortInstruction(runCtx, "MAP_SET", chunk, offset);
		case OP_GET_SUPER:
			return shortInstruction(runCtx, "GET_SUPER", chunk, offset);
		case OP_EQUAL:
			return simpleInstruction(runCtx, "EQUAL", offset);
		case OP_GREATER:
			return simpleInstruction(runCtx, "GREATER", offset);
		case OP_LESS:
			return simpleInstruction(runCtx, "LESS", offset);
		case OP_ADD:
			return simpleInstruction(runCtx, "ADD", offset);
		case OP_SUBTRACT:
			return simpleInstruction(runCtx, "SUBTRACT", offset);
		case OP_MULTIPLY:
			return simpleInstruction(runCtx, "MULTIPLY", offset);
		case OP_DIVIDE:
			return simpleInstruction(runCtx, "DIVIDE", offset);
		case OP_MODULO:
			return simpleInstruction(runCtx, "MODULO", offset);
		case OP_INSTANCEOF:
			return simpleInstruction(runCtx, "INSTANCEOF", offset);
		case OP_NOT:
			return simpleInstruction(runCtx, "NOT", offset);
		case OP_NEGATE:
			return simpleInstruction(runCtx, "NEGATE", offset);
		case OP_JUMP:
			return jumpInstruction(runCtx, "JUMP", 1, chunk, offset);
		case OP_JUMP_IF_FALSE:
			return jumpInstruction(runCtx, "JUMP_IF_FALSE", 1, chunk, offset);
		case OP_LOOP:
			return jumpInstruction(runCtx, "LOOP", -1, chunk, offset);
		case OP_CALL:
			return callInstruction(runCtx, "CALL", chunk, offset);
		case OP_INVOKE:
			return invokeInstruction(runCtx, "INVOKE", chunk, offset);
		case OP_MEMBER_INVOKE:
			return memberInvokeInstruction(runCtx, "MEMBER_INVOKE", chunk, offset);
		case OP_SUPER_INVOKE:
			return invokeInstruction(runCtx, "SUPER_INVOKE", chunk, offset);
		case OP_SUPER_INIT:
			return callInstruction(runCtx, "SUPER_INIT", chunk, offset);
		case OP_CLOSURE: {
			offset++;
			uint16_t constant;
			memcpy(&constant, chunk->code + offset, sizeof(uint16_t));
			offset += 2;
			eloxPrintf(runCtx, ELOX_IO_DEBUG, "%-22s %4d ", "CLOSURE", constant);
			printValue(runCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
			ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");

			ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
			for (int j = 0; j < function->upvalueCount; j++) {
				int isLocal = chunk->code[offset++];
				int index = chunk->code[offset++];
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "%04d    |                     %s %d\n",
						   offset - 2, isLocal ? "local" : "upvalue", index);
			}

			return offset;
		}
		case OP_CLOSE_UPVALUE:
			return simpleInstruction(runCtx, "CLOSE_UPVALUE", offset);
		case OP_RETURN:
			return simpleInstruction(runCtx, "RETURN", offset);
		case OP_END:
			return simpleInstruction(runCtx, "END", offset);
		case OP_CLASS:
			return constantUShortInstruction(runCtx, "CLASS", chunk, offset);
		case OP_ANON_CLASS:
			return simpleInstruction(runCtx, "ANON_CLASS", offset);
		case OP_INHERIT:
			return simpleInstruction(runCtx, "INHERIT", offset);
		case OP_METHOD:
			return constantUShortInstruction(runCtx, "METHOD", chunk, offset);
		case OP_FIELD:
			return constantUShortInstruction(runCtx, "FIELD", chunk, offset);
		case OP_STATIC:
			return constantUShortInstruction(runCtx, "STATIC", chunk, offset);
		case OP_RESOLVE_MEMBERS:
			return resolveMembersInstruction(runCtx, "RESOLVE_MEMBERS", chunk, offset);
		case OP_ARRAY_BUILD:
			return arrayBuildInstruction(runCtx, "ARRAY_BUILD", chunk, offset);
		case OP_INDEX:
			return simpleInstruction(runCtx, "INDEX", offset);
		case OP_INDEX_STORE:
			return simpleInstruction(runCtx, "INDEX_STORE", offset);
		case OP_SLICE:
			return simpleInstruction(runCtx, "SLICE", offset);
		case OP_MAP_BUILD:
			return shortInstruction(runCtx, "MAP_BUILD", chunk, offset);
		case OP_THROW:
			return simpleInstruction(runCtx, "THROW", offset);
		case OP_PUSH_EXH:
			return exceptionHandlerInstruction(runCtx, "PUSH_EXH", chunk, offset);
		case OP_UNROLL_EXH:
			return byteInstruction(runCtx, "UNROLL_EXH", chunk, offset);
		case OP_UNROLL_EXH_R:
			return byteInstruction(runCtx, "UNROLL_EXH_R", chunk, offset);
		case OP_UNROLL_EXH_F:
			return byteInstruction(runCtx, "UNROLL_EXH_F", chunk, offset);
		case OP_FOREACH_INIT:
			return forEachInstruction(runCtx, "FOREACH_INIT", chunk, offset);
		case OP_UNPACK:
			return unpackInstruction(runCtx, "UNPACK", chunk, offset);
		case OP_IMPORT:
			return importInstruction(runCtx, "IMPORT", chunk, offset);
		case OP_DATA:
			return dataInstruction(runCtx, "DATA", chunk, offset);
		default:
			eloxPrintf(runCtx, ELOX_IO_DEBUG, "Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}

#define BASIC_TOKEN(tok, name) \
	case TOKEN_ ## tok: \
		eloxPrintf(runCtx, ELOX_IO_DEBUG, name); \
		break

#define STRING_TOKEN(tok, name) \
	case TOKEN_ ## tok: \
		eloxPrintf(runCtx, ELOX_IO_DEBUG,  name "[%.*s]", token->string.length, token->string.chars); \
		break

void printToken(RunCtx *runCtx, Token *token) {
	switch (token->type) {
		BASIC_TOKEN(LEFT_PAREN, "(");
		BASIC_TOKEN(RIGHT_PAREN, ")");
		BASIC_TOKEN(LEFT_BRACE, "{");
		BASIC_TOKEN(RIGHT_BRACE, "}");
		BASIC_TOKEN(LEFT_BRACKET, "[");
		BASIC_TOKEN(RIGHT_BRACKET, "]");
		BASIC_TOKEN(COLON, ":");
		BASIC_TOKEN(DOUBLE_COLON, "::");
		BASIC_TOKEN(COMMA, ",");
		BASIC_TOKEN(DOT, ".");
		BASIC_TOKEN(DOT_DOT, "..");
		BASIC_TOKEN(ELLIPSIS, "...");
		BASIC_TOKEN(MINUS, "-");
		BASIC_TOKEN(PERCENT, "%%");
		BASIC_TOKEN(PLUS, "+");
		BASIC_TOKEN(SEMICOLON, ";");
		BASIC_TOKEN(SLASH, "/");
		BASIC_TOKEN(STAR, "*");

		BASIC_TOKEN(BANG, "!");
		BASIC_TOKEN(BANG_EQUAL, "!=");
		BASIC_TOKEN(EQUAL, "=");
		BASIC_TOKEN(EQUAL_EQUAL, "==");
		BASIC_TOKEN(COLON_EQUAL, ":=");
		BASIC_TOKEN(GREATER, ">");
		BASIC_TOKEN(GREATER_EQUAL, ">=");
		BASIC_TOKEN(LESS, "<");
		BASIC_TOKEN(LESS_EQUAL, "<=");
		BASIC_TOKEN(PLUS_EQUAL, "+=");
		BASIC_TOKEN(MINUS_EQUAL, "-=");
		BASIC_TOKEN(SLASH_EQUAL, "/=");
		BASIC_TOKEN(STAR_EQUAL, "*=");
		BASIC_TOKEN(PERCENT_EQUAL, "%%=");

		STRING_TOKEN(IDENTIFIER, "IDENTIFIER");
		STRING_TOKEN(STRING, "STRING");
		STRING_TOKEN(NUMBER, "NUMBER");

		STRING_TOKEN(FSTRING_START, "FSTRING_START");
		STRING_TOKEN(FSTRING, "FSTRING");
		STRING_TOKEN(FSTRING_END, "FSTRING_END");

		BASIC_TOKEN(AND, "AND");
		BASIC_TOKEN(BREAK, "BREAK");
		BASIC_TOKEN(CATCH, "CATCH");
		BASIC_TOKEN(CONTINUE, "CONTINUE");
		BASIC_TOKEN(CLASS, "CLASS");
		BASIC_TOKEN(ELSE, "ELSE");
		BASIC_TOKEN(FALSE, "FALSE");
		BASIC_TOKEN(FINALLY, "FINALLY");
		BASIC_TOKEN(FOR, "FOR");
		BASIC_TOKEN(FOREACH, "FOREACH");
		BASIC_TOKEN(FROM, "FROM");
		BASIC_TOKEN(FUNCTION, "FUNCTION");
		BASIC_TOKEN(GLOBAL, "GLOBAL");
		BASIC_TOKEN(IF, "IF");
		BASIC_TOKEN(IMPORT, "IMPORT");
		BASIC_TOKEN(IN, "IN");
		BASIC_TOKEN(INSTANCEOF, "INSTANCEOF");
		BASIC_TOKEN(LOCAL, "LOCAL");
		BASIC_TOKEN(NIL, "NIL");
		BASIC_TOKEN(OR, "OR");
		BASIC_TOKEN(RETURN, "RETURN");
		BASIC_TOKEN(SUPER, "SUPER");
		BASIC_TOKEN(THIS, "THIS");
		BASIC_TOKEN(THROW, "THROW");
		BASIC_TOKEN(TRUE, "TRUE");
		BASIC_TOKEN(TRY, "TRY");
		BASIC_TOKEN(WHILE, "WHILE");

		STRING_TOKEN(ERROR, "ERROR");
		BASIC_TOKEN(EOF, "EOF");
	}
}
