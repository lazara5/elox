// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/debug.h"
#include "elox/object.h"
#include "elox/compiler.h"
#include <elox/state.h>
#include <elox/vm.h>

#include <string.h>

void disassembleChunk(VMCtx *vmCtx, Chunk *chunk, const char *name) {
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "== %s ==\n", name);

	for (int offset = 0; offset < chunk->count; )
		offset = disassembleInstruction(vmCtx, chunk, offset);

	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "== END CHUNK ==\n");
}

static int constantByteInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant = chunk->code[offset + 1];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 2;
}

static int constantUShortInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 3;
}

static int globalInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, constant);
	return offset + 3;
}

static int getPropertyInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 3;
}

static int invokeInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t argCount = chunk->code[offset + 3];
	uint8_t hasExpansions = chunk->code[offset + 4];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s (%d args %d) %4d (", name, argCount, hasExpansions, constant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")\n");
	return offset + 5;
}

static int memberInvokeInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t slot;
	memcpy(&slot, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t argCount = chunk->code[offset + 3];
	uint8_t hasExpansions = chunk->code[offset + 4];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s (%d args %d) %u\n", name, argCount, hasExpansions, slot);
	return offset + 5;
}

static int simpleInstruction(VMCtx *vmCtx, const char *name, int offset) {
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%s\n", name);
	return offset + 1;
}

static int byteInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d\n", name, slot);
	return offset + 2;
}

static int shortInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t slot;
	memcpy(&slot, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, slot);
	return offset + 3;
}

static int jumpInstruction(VMCtx *vmCtx, const char *name, int sign, Chunk *chunk, int offset) {
	uint16_t jump;
	memcpy(&jump, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
	return offset + 3;
}

static int callInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t numArgs = chunk->code[offset + 1];
	uint8_t hasExpansions = chunk->code[offset + 2];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d %4d\n", name, numArgs, hasExpansions);
	return offset + 3;
}

static int exceptionHandlerInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t stackLevel = chunk->code[offset + 1];
	uint16_t handlerData;
	memcpy(&handlerData, &chunk->code[offset + 2], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s [%d] @%d\n", name, stackLevel, handlerData);
	return offset + 4;
}

static int arrayBuildInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t objType = chunk->code[offset + 1];
	uint16_t numItems;
	memcpy(&numItems, &chunk->code[offset + 2], sizeof(uint16_t));
	const char *type = (objType == OBJ_ARRAY) ? "ARRAY" : "TUPLE";
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %s [%d]\n", name, type, numItems);
	return offset + 4;
}

static int forEachInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t hasNextSlot = chunk->code[offset + 1];
	bool hasNextPostArgs = chunk->code[offset + 2];
	uint8_t nextSlot = chunk->code[offset + 3];
	bool nextPostArgs = chunk->code[offset + 4];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d %4d\n", name, hasNextSlot, nextSlot);
	return offset + 5;
}

static int unpackInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t numVal = chunk->code[offset + 1];
	bool first = true;
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s ", name);
	int argOffset = offset + 2;
	int argSize = 0;
	for (int i = 0; i < numVal; i++) {
		if (!first)
			ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ", ");
		first = false;

		VarType varType = chunk->code[argOffset];
		switch (varType) {
			case VAR_LOCAL:
				eloxPrintf(vmCtx, ELOX_IO_DEBUG, "L %d %s",
							chunk->code[argOffset + 1], chunk->code[argOffset + 1] ? "POST" : "PRE");
				argSize += 3;
				argOffset += 3;
				break;
			case VAR_UPVALUE:
				eloxPrintf(vmCtx, ELOX_IO_DEBUG, "U %d", chunk->code[argOffset + 1]);
				argSize += 2;
				argOffset += 2;
				break;
			case VAR_GLOBAL: {
				uint16_t slot;
				memcpy(&slot, &chunk->code[argOffset + 1], sizeof(uint16_t));
				eloxPrintf(vmCtx, ELOX_IO_DEBUG, "G %d", slot);
				argSize += 3;
				argOffset += 3;
				break;
			}
		}
	}
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");
	return offset + 1 + 1 + argSize;
}

static int resolveMembersInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t numMembers;
	memcpy(&numMembers, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s\n", name);

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
		eloxPrintf(vmCtx, ELOX_IO_DEBUG,
				   "        |                        [%u]=%s[%s %u (",
				   slot, super ? "super" : "this",
				   strMask[mask - 1], nameIndex);
		printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[nameIndex]);
		ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")]\n");
	}

	return offset + 3 + 5 * numMembers;
}

static int dataInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t len = chunk->code[offset + 1];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s [%d]=[", name, len);
	for (int i = 0; i < len; i++) {
		if (i == 0)
			eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%d", chunk->code[offset + 2 + i]);
		else
			eloxPrintf(vmCtx, ELOX_IO_DEBUG, ",%d", chunk->code[offset + 2 + i]);
	}
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "]\n");
	return offset + 1 + len + 1;
}

static int localInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	uint8_t postArgs = chunk->code[offset + 2];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d %s\n", name, slot, postArgs ? "POST" : "PRE");
	return offset + 3;
}

static int importInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t module;
	memcpy(&module, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s ", name);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[module]);
	uint16_t numArgs;
	memcpy(&numArgs, &chunk->code[offset + 3], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, " %u (", numArgs);
	for (int i = 0; i < numArgs; i++) {
		uint16_t sym;
		if (i > 0)
			ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ", ");
		memcpy(&sym, &chunk->code[offset + 5 + 2 * i], sizeof(uint16_t));
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%u", sym);
	}
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")");

	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");
	return offset + 5 + 2 * numArgs;
}

int disassembleInstruction(VMCtx *vmCtx, Chunk *chunk, int offset) {
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%04d ", offset);
	int line = getLine(chunk, offset);
	if (offset > 0 && line == getLine(chunk, offset - 1)) {
		ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "   | ");
	} else {
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%4d ", line);
	}

	uint8_t instruction = chunk->code[offset];
	switch (instruction) {
		case OP_CONST8:
			return constantByteInstruction(vmCtx, "CONST8", chunk, offset);
		case OP_CONST16:
			return constantUShortInstruction(vmCtx, "CONST16", chunk, offset);
		case OP_IMMI:
			return shortInstruction(vmCtx, "IMMI", chunk, offset);
		case OP_NIL:
			return simpleInstruction(vmCtx, "NIL", offset);
		case OP_TRUE:
			return simpleInstruction(vmCtx, "TRUE", offset);
		case OP_FALSE:
			return simpleInstruction(vmCtx, "FALSE", offset);
		case OP_POP:
			return simpleInstruction(vmCtx, "POP", offset);
		case OP_POPN:
			return byteInstruction(vmCtx, "POPN", chunk, offset);
		case OP_SWAP:
			return simpleInstruction(vmCtx, "SWAP", offset);
		case OP_NUM_VARARGS:
			return simpleInstruction(vmCtx, "NUM_VARARGS", offset);
		case OP_EXPAND_VARARGS:
			return byteInstruction(vmCtx, "EXPAND_VARARGS", chunk, offset);
		case OP_PEEK:
			return byteInstruction(vmCtx, "PEEK", chunk, offset);
		case OP_GET_LOCAL:
			 return localInstruction(vmCtx, "GET_LOCAL", chunk, offset);
		case OP_GET_VARARG:
			return simpleInstruction(vmCtx, "GET_VARARG", offset);
		case OP_SET_LOCAL:
			return localInstruction(vmCtx, "SET_LOCAL", chunk, offset);
		case OP_SET_VARARG:
			return simpleInstruction(vmCtx, "SET_VARARG", offset);
		case OP_GET_GLOBAL:
			return globalInstruction(vmCtx, "GET_GLOBAL", chunk, offset);
		case OP_DEFINE_GLOBAL:
			return globalInstruction(vmCtx, "DEFINE_GLOBAL", chunk, offset);
		case OP_SET_GLOBAL:
			return globalInstruction(vmCtx, "SET_GLOBAL", chunk, offset);
		case OP_GET_UPVALUE:
			return byteInstruction(vmCtx, "GET_UPVALUE", chunk, offset);
		case OP_SET_UPVALUE:
			return byteInstruction(vmCtx, "SET_UPVALUE", chunk, offset);
		case OP_GET_PROP:
			return getPropertyInstruction(vmCtx, "GET_PROP", chunk, offset);
		case OP_MAP_GET:
			return shortInstruction(vmCtx, "MAP_GET", chunk, offset);
		case OP_GET_MEMBER_PROP:
			return shortInstruction(vmCtx, "GET_MEMBER_PROP", chunk, offset);
		case OP_SET_PROP:
			return constantUShortInstruction(vmCtx, "SET_PROP", chunk, offset);
		case OP_SET_MEMBER_PROP:
			return shortInstruction(vmCtx, "SET_MEMBER_PROP", chunk, offset);
		case OP_MAP_SET:
			return constantUShortInstruction(vmCtx, "MAP_SET", chunk, offset);
		case OP_GET_SUPER:
			return shortInstruction(vmCtx, "GET_SUPER", chunk, offset);
		case OP_EQUAL:
			return simpleInstruction(vmCtx, "EQUAL", offset);
		case OP_GREATER:
			return simpleInstruction(vmCtx, "GREATER", offset);
		case OP_LESS:
			return simpleInstruction(vmCtx, "LESS", offset);
		case OP_ADD:
			return simpleInstruction(vmCtx, "ADD", offset);
		case OP_SUBTRACT:
			return simpleInstruction(vmCtx, "SUBTRACT", offset);
		case OP_MULTIPLY:
			return simpleInstruction(vmCtx, "MULTIPLY", offset);
		case OP_DIVIDE:
			return simpleInstruction(vmCtx, "DIVIDE", offset);
		case OP_MODULO:
			return simpleInstruction(vmCtx, "MODULO", offset);
		case OP_INSTANCEOF:
			return simpleInstruction(vmCtx, "INSTANCEOF", offset);
		case OP_NOT:
			return simpleInstruction(vmCtx, "NOT", offset);
		case OP_NEGATE:
			return simpleInstruction(vmCtx, "NEGATE", offset);
		case OP_JUMP:
			return jumpInstruction(vmCtx, "JUMP", 1, chunk, offset);
		case OP_JUMP_IF_FALSE:
			return jumpInstruction(vmCtx, "JUMP_IF_FALSE", 1, chunk, offset);
		case OP_LOOP:
			return jumpInstruction(vmCtx, "LOOP", -1, chunk, offset);
		case OP_CALL:
			return callInstruction(vmCtx, "CALL", chunk, offset);
		case OP_INVOKE:
			return invokeInstruction(vmCtx, "INVOKE", chunk, offset);
		case OP_MEMBER_INVOKE:
			return memberInvokeInstruction(vmCtx, "MEMBER_INVOKE", chunk, offset);
		case OP_SUPER_INVOKE:
			return invokeInstruction(vmCtx, "SUPER_INVOKE", chunk, offset);
		case OP_SUPER_INIT:
			return callInstruction(vmCtx, "SUPER_INIT", chunk, offset);
		case OP_CLOSURE: {
			offset++;
			uint16_t constant;
			memcpy(&constant, chunk->code + offset, sizeof(uint16_t));
			offset += 2;
			eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d ", "CLOSURE", constant);
			printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
			ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");

			ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
			for (int j = 0; j < function->upvalueCount; j++) {
				int isLocal = chunk->code[offset++];
				int index = chunk->code[offset++];
				eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%04d    |                     %s %d\n",
						   offset - 2, isLocal ? "local" : "upvalue", index);
			}

			return offset;
		}
		case OP_CLOSE_UPVALUE:
			return simpleInstruction(vmCtx, "CLOSE_UPVALUE", offset);
		case OP_RETURN:
			return simpleInstruction(vmCtx, "RETURN", offset);
		case OP_CLASS:
			return constantUShortInstruction(vmCtx, "CLASS", chunk, offset);
		case OP_ANON_CLASS:
			return simpleInstruction(vmCtx, "ANON_CLASS", offset);
		case OP_INHERIT:
			return simpleInstruction(vmCtx, "INHERIT", offset);
		case OP_METHOD:
			return constantUShortInstruction(vmCtx, "METHOD", chunk, offset);
		case OP_FIELD:
			return constantUShortInstruction(vmCtx, "FIELD", chunk, offset);
		case OP_STATIC:
			return constantUShortInstruction(vmCtx, "STATIC", chunk, offset);
		case OP_RESOLVE_MEMBERS:
			return resolveMembersInstruction(vmCtx, "RESOLVE_MEMBERS", chunk, offset);
		case OP_ARRAY_BUILD:
			return arrayBuildInstruction(vmCtx, "ARRAY_BUILD", chunk, offset);
		case OP_INDEX:
			return simpleInstruction(vmCtx, "INDEX", offset);
		case OP_INDEX_STORE:
			return simpleInstruction(vmCtx, "INDEX_STORE", offset);
		case OP_SLICE:
			return simpleInstruction(vmCtx, "SLICE", offset);
		case OP_MAP_BUILD:
			return shortInstruction(vmCtx, "MAP_BUILD", chunk, offset);
		case OP_THROW:
			return simpleInstruction(vmCtx, "THROW", offset);
		case OP_PUSH_EXCEPTION_HANDLER:
			return exceptionHandlerInstruction(vmCtx, "PUSH_EXCEPTION_HANDLER", chunk, offset);
		case OP_POP_EXCEPTION_HANDLER:
			return byteInstruction(vmCtx, "POP_EXCEPTION_HANDLER", chunk, offset);
		case OP_FOREACH_INIT:
			return forEachInstruction(vmCtx, "FOREACH_INIT", chunk, offset);
		case OP_UNPACK:
			return unpackInstruction(vmCtx, "UNPACK", chunk, offset);
		case OP_IMPORT:
			return importInstruction(vmCtx, "IMPORT", chunk, offset);
		case OP_DATA:
			return dataInstruction(vmCtx, "DATA", chunk, offset);
		default:
			eloxPrintf(vmCtx, ELOX_IO_DEBUG, "Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}
