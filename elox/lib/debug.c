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

static int builtinInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, constant);
	return offset + 3;
}

static int classInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t nameConstant;
	memcpy(&nameConstant, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t abstract = chunk->code[offset + 3];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, nameConstant);
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ") %u\n", abstract);
	return offset + 4;
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

static int invokeRefInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t refSlot;
	memcpy(&refSlot, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t argCount = chunk->code[offset + 3];
	uint8_t hasExpansions = chunk->code[offset + 4];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s (%d args %d) %u\n",
			   name, argCount, hasExpansions, refSlot);
	return offset + 5;
}

static int simpleInstruction(VMCtx *vmCtx, const char *name, int offset) {
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%s\n", name);
	return offset + 1;
}

static int byteInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t val = chunk->code[offset + 1];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d\n", name, val);
	return offset + 2;
}

static int shortInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t val;
	memcpy(&val, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, val);
	return offset + 3;
}

static int intInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	int32_t val;
	memcpy(&val, &chunk->code[offset + 1], sizeof(int32_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d\n", name, val);
	return offset + 5;
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

static int inheritInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t numSuper = chunk->code[offset + 1];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d\n", name, numSuper);
	return offset + 2;
}

static int pushExhInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t handlerData;
	memcpy(&handlerData, &chunk->code[offset + 1], sizeof(uint16_t));
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s @%d\n", name, handlerData);
	return offset + 3;
}

static int unrollExhInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t hc = chunk->code[offset + 1];
	uint8_t preserve = chunk->code[offset + 2];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4u %4u\n", name, hc, preserve);
	return offset + 3;
}

static int newArrayInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset,
							   ObjType objType) {
	uint16_t numItems;
	memcpy(&numItems, &chunk->code[offset + 2], sizeof(uint16_t));
	const char *type = (objType == OBJ_ARRAY) ? "ARRAY" : "TUPLE";
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %s [%d]\n", name, type, numItems);
	return offset + 3;
}

static int forEachInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint8_t hasNextSlot = chunk->code[offset + 1];
	bool hasNextPostArgs = chunk->code[offset + 2];
	uint8_t nextSlot = chunk->code[offset + 3];
	bool nextPostArgs = chunk->code[offset + 4];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %4d %4d\n", name, hasNextSlot, nextSlot);
	return offset + 5;
}

static int absMethodInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t constant;
	memcpy(&constant, &chunk->code[offset + 1], sizeof(uint16_t));
	uint8_t parentOffset = chunk->code[offset + 3];
	uint8_t arity = chunk->code[offset + 4];
	uint8_t hasVarargs = chunk->code[offset + 5];
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, constant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[constant]);
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ") %u %u %u\n", parentOffset, arity, hasVarargs);
	return offset + 6;
}

static int defaultMethodInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t nameConstant;
	memcpy(&nameConstant, &chunk->code[offset + 1], sizeof(uint16_t));
	uint16_t functionConstant;
	memcpy(&functionConstant, &chunk->code[offset + 3], sizeof(uint16_t));
	uint16_t numRef;
	memcpy(&numRef, &chunk->code[offset + 5], sizeof(uint16_t));

	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, nameConstant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[nameConstant]);
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ") %u (", functionConstant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[functionConstant]);
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ") %u\n", numRef);

	Chunk *functionChunk =
		&((ObjFunction *)AS_OBJ(chunk->constants.values[functionConstant]))->chunk;

	for (uint16_t i = 0; i < numRef; i++) {
		int32_t off;
		memcpy(&off, &chunk->code[offset + 7 + (7 * i)], sizeof(uint32_t));
		uint8_t slotType = chunk->code[offset + 7 + (7 * i) + 4];
		bool super = slotType & 0x1;
		uint8_t propType = (slotType & 0x6) >> 1;
		uint16_t name;
		memcpy(&name, &chunk->code[offset + 7 + (7 * i) + 5], sizeof(uint16_t));
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "        %5d [%s|%u] %d(", off,
				   super ? "SUPER" : "THIS", propType, name);
		printValue(vmCtx, ELOX_IO_DEBUG, functionChunk->constants.values[name]);
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, ")\n");
	}

	return offset + 7 + (numRef * 7);
}

static int methodInstruction(VMCtx *vmCtx, const char *name, Chunk *chunk, int offset) {
	uint16_t nameConstant;
	memcpy(&nameConstant, &chunk->code[offset + 1], sizeof(uint16_t));
	uint16_t functionConstant;
	memcpy(&functionConstant, &chunk->code[offset + 3], sizeof(uint16_t));
	uint16_t numRef;
	memcpy(&numRef, &chunk->code[offset + 5], sizeof(uint16_t));

	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%-22s %5d (", name, nameConstant);
	printValue(vmCtx, ELOX_IO_DEBUG, chunk->constants.values[nameConstant]);
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ") %u\n", numRef);

	Chunk *functionChunk =
		&((ObjFunction *)AS_OBJ(chunk->constants.values[functionConstant]))->chunk;

	for (uint16_t i = 0; i < numRef; i++) {
		int32_t off;
		memcpy(&off, &chunk->code[offset + 7 + (7 * i)], sizeof(uint32_t));
		uint8_t slotType = chunk->code[offset + 7 + (7 * i) + 4];
		bool super = slotType & 0x1;
		uint8_t propType = (slotType & 0x6) >> 1;
		uint16_t name;
		memcpy(&name, &chunk->code[offset + 7 + (7 * i) + 5], sizeof(uint16_t));
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "        %5d [%s|%u] %d(", off,
				   super ? "SUPER" : "THIS", propType, name);
		printValue(vmCtx, ELOX_IO_DEBUG, functionChunk->constants.values[name]);
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, ")\n");
	}

	return offset + 7 + (numRef * 7);
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

		uint8_t type = chunk->code[argOffset];
		VarScope varType = type & 0xF;
		bool isRest = (type & 0xF0) != 0;
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
			case VAR_BUILTIN: {
				uint16_t slot;
				memcpy(&slot, &chunk->code[argOffset + 1], sizeof(uint16_t));
				eloxPrintf(vmCtx, ELOX_IO_DEBUG, "B %d", slot);
				argSize += 3;
				argOffset += 3;
				break;
			case VAR_TUPLE:
				ELOX_UNREACHABLE();
				break;
			}
		}
		if (isRest)
			ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, " ...");
	}
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");
	return offset + 1 + 1 + argSize;
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
			return intInstruction(vmCtx, "IMMI", chunk, offset);
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
		case OP_EXPAND:
			return byteInstruction(vmCtx, "EXPAND", chunk, offset);
		case OP_PEEK:
			return byteInstruction(vmCtx, "PEEK", chunk, offset);
		case OP_GET_LOCAL:
			 return localInstruction(vmCtx, "GET_LOCAL", chunk, offset);
		case OP_GET_VARARGS:
			return simpleInstruction(vmCtx, "GET_VARARGS", offset);
		case OP_SET_LOCAL:
			return localInstruction(vmCtx, "SET_LOCAL", chunk, offset);
		case OP_GET_GLOBAL:
			return globalInstruction(vmCtx, "GET_GLOBAL", chunk, offset);
		case OP_GET_BUILTIN:
			return builtinInstruction(vmCtx, "GET_BUILTIN", chunk, offset);
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
		case OP_GET_REF:
			return shortInstruction(vmCtx, "GET_REF", chunk, offset);
		case OP_MAP_GET:
			return shortInstruction(vmCtx, "MAP_GET", chunk, offset);
		case OP_SET_PROP:
			return constantUShortInstruction(vmCtx, "SET_PROP", chunk, offset);
		case OP_SET_REF:
			return shortInstruction(vmCtx, "SET_REF", chunk, offset);
		case OP_MAP_SET:
			return constantUShortInstruction(vmCtx, "MAP_SET", chunk, offset);
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
		case OP_IN:
			return simpleInstruction(vmCtx, "IN", offset);
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
		case OP_INVOKE_REF:
			return invokeRefInstruction(vmCtx, "INVOKE_REF", chunk, offset);
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

			ObjFunction *function = (ObjFunction *)AS_OBJ(chunk->constants.values[constant]);
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
		case OP_END:
			return simpleInstruction(vmCtx, "END", offset);
		case OP_INTF:
			return constantUShortInstruction(vmCtx, "INTF", chunk, offset);
		case OP_CLASS:
			return classInstruction(vmCtx, "CLASS", chunk, offset);
		case OP_INHERIT:
			return inheritInstruction(vmCtx, "INHERIT", chunk, offset);
		case OP_ABS_METHOD:
			return absMethodInstruction(vmCtx, "ABS_METHOD", chunk, offset);
		case OP_DEFAULT_METHOD:
			return defaultMethodInstruction(vmCtx, "DEFAULT_METHOD", chunk, offset);
		case OP_METHOD:
			return methodInstruction(vmCtx, "METHOD", chunk, offset);
		case OP_FIELD:
			return constantUShortInstruction(vmCtx, "FIELD", chunk, offset);
		case OP_STATIC:
			return constantUShortInstruction(vmCtx, "STATIC", chunk, offset);
		case OP_CLOSE_CLASS:
			return simpleInstruction(vmCtx, "CLOSE_CLASS", offset);
		case OP_NEW_ARRAY:
			return newArrayInstruction(vmCtx, "NEW_ARRAY", chunk, offset, OBJ_ARRAY);
		case OP_NEW_TUPLE:
			return newArrayInstruction(vmCtx, "NEW_TUPLE", chunk, offset, OBJ_TUPLE);
		case OP_INDEX:
			return simpleInstruction(vmCtx, "INDEX", offset);
		case OP_INDEX_STORE:
			return simpleInstruction(vmCtx, "INDEX_STORE", offset);
		case OP_SLICE:
			return simpleInstruction(vmCtx, "SLICE", offset);
		case OP_NEW_MAP:
			return shortInstruction(vmCtx, "NEW_MAP", chunk, offset);
		case OP_THROW:
			return simpleInstruction(vmCtx, "THROW", offset);
		case OP_PUSH_EXH:
			return pushExhInstruction(vmCtx, "PUSH_EXH", chunk, offset);
		case OP_UNROLL_EXH:
			return unrollExhInstruction(vmCtx, "UNROLL_EXH", chunk, offset);
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

#define CASE_BASIC_TOKEN(tok, name) \
	case TOKEN_ ## tok: \
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, name); \
		break

#define CASE_STRING_TOKEN(tok, name) \
	case TOKEN_ ## tok: \
		eloxPrintf(vmCtx, ELOX_IO_DEBUG,  name "[%.*s]", token->string.length, token->string.chars); \
		break

void printToken(VMCtx *vmCtx, Token *token) {
	switch (token->type) {
		CASE_BASIC_TOKEN(LEFT_PAREN, "(");
		CASE_BASIC_TOKEN(RIGHT_PAREN, ")");
		CASE_BASIC_TOKEN(LEFT_BRACE, "{");
		CASE_BASIC_TOKEN(RIGHT_BRACE, "}");
		CASE_BASIC_TOKEN(LEFT_BRACKET, "[");
		CASE_BASIC_TOKEN(RIGHT_BRACKET, "]");
		CASE_BASIC_TOKEN(COLON, ":");
		CASE_BASIC_TOKEN(DOUBLE_COLON, "::");
		CASE_BASIC_TOKEN(COMMA, ",");
		CASE_BASIC_TOKEN(DOT, ".");
		CASE_BASIC_TOKEN(DOT_DOT, "..");
		CASE_BASIC_TOKEN(ELLIPSIS, "...");
		CASE_BASIC_TOKEN(MINUS, "-");
		CASE_BASIC_TOKEN(PERCENT, "%%");
		CASE_BASIC_TOKEN(PLUS, "+");
		CASE_BASIC_TOKEN(SEMICOLON, ";");
		CASE_BASIC_TOKEN(SLASH, "/");
		CASE_BASIC_TOKEN(STAR, "*");

		CASE_BASIC_TOKEN(BANG, "!");
		CASE_BASIC_TOKEN(BANG_EQUAL, "!=");
		CASE_BASIC_TOKEN(EQUAL, "=");
		CASE_BASIC_TOKEN(EQUAL_EQUAL, "==");
		CASE_BASIC_TOKEN(COLON_EQUAL, ":=");
		CASE_BASIC_TOKEN(GREATER, ">");
		CASE_BASIC_TOKEN(GREATER_EQUAL, ">=");
		CASE_BASIC_TOKEN(LESS, "<");
		CASE_BASIC_TOKEN(LESS_EQUAL, "<=");
		CASE_BASIC_TOKEN(PLUS_EQUAL, "+=");
		CASE_BASIC_TOKEN(MINUS_EQUAL, "-=");
		CASE_BASIC_TOKEN(SLASH_EQUAL, "/=");
		CASE_BASIC_TOKEN(STAR_EQUAL, "*=");
		CASE_BASIC_TOKEN(PERCENT_EQUAL, "%%=");

		CASE_STRING_TOKEN(IDENTIFIER, "IDENTIFIER");
		CASE_STRING_TOKEN(STRING, "STRING");
		CASE_STRING_TOKEN(NUMBER, "NUMBER");

		CASE_STRING_TOKEN(FSTRING_START, "FSTRING_START");
		CASE_STRING_TOKEN(FSTRING, "FSTRING");
		CASE_STRING_TOKEN(FSTRING_END, "FSTRING_END");

		CASE_BASIC_TOKEN(AND, "AND");
		CASE_BASIC_TOKEN(BREAK, "BREAK");
		CASE_BASIC_TOKEN(CATCH, "CATCH");
		CASE_BASIC_TOKEN(CONTINUE, "CONTINUE");
		CASE_BASIC_TOKEN(CLASS, "CLASS");
		CASE_BASIC_TOKEN(ELSE, "ELSE");
		CASE_BASIC_TOKEN(EXTENDS, "EXTENDS");
		CASE_BASIC_TOKEN(FALSE, "FALSE");
		CASE_BASIC_TOKEN(FINALLY, "FINALLY");
		CASE_BASIC_TOKEN(FOR, "FOR");
		CASE_BASIC_TOKEN(FROM, "FROM");
		CASE_BASIC_TOKEN(FUNCTION, "FUNCTION");
		CASE_BASIC_TOKEN(IF, "IF");
		CASE_BASIC_TOKEN(IMPLEMENTS, "IMPLEMENTS");
		CASE_BASIC_TOKEN(IMPORT, "IMPORT");
		CASE_BASIC_TOKEN(IN, "IN");
		CASE_BASIC_TOKEN(INSTANCEOF, "INSTANCEOF");
		CASE_BASIC_TOKEN(INTERFACE, "INTERFACE");
		CASE_BASIC_TOKEN(NIL, "NIL");
		CASE_BASIC_TOKEN(OR, "OR");
		CASE_BASIC_TOKEN(RETURN, "RETURN");
		CASE_BASIC_TOKEN(SUPER, "SUPER");
		CASE_BASIC_TOKEN(THIS, "THIS");
		CASE_BASIC_TOKEN(THROW, "THROW");
		CASE_BASIC_TOKEN(TRUE, "TRUE");
		CASE_BASIC_TOKEN(TRY, "TRY");
		CASE_BASIC_TOKEN(WHILE, "WHILE");

		CASE_BASIC_TOKEN(ABSTRACT, "ABSTRACT");
		CASE_BASIC_TOKEN(GLOBAL, "GLOBAL");
		CASE_BASIC_TOKEN(LOCAL, "LOCAL");

		CASE_STRING_TOKEN(ERROR, "ERROR");
		CASE_BASIC_TOKEN(EOF, "EOF");
	}
}
