#include <stdio.h>

#include "elox/debug.h"
#include "elox/object.h"
#include "elox/compiler.h"

void disassembleChunk(Chunk *chunk, const char *name) {
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count; )
		offset = disassembleInstruction(chunk, offset);

	printf("== END CHUNK ==\n");
}

static int constantInstruction(const char *name, Chunk *chunk, int offset, int numBytes) {
	uint16_t constant = chunk->code[offset + 1];
	for (int i = 1; i < numBytes; i++) {
		constant <<= 8;
		constant |= chunk->code[offset + i + 1];
	}
	printf("%-22s %5d (", name, constant);
	printValue(chunk->constants.values[constant]);
	printf(")\n");
	return offset + 1 + numBytes;
}

static int globalInstruction(const char *name, Chunk *chunk, int offset, int numBytes) {
	uint16_t constant = chunk->code[offset + 1];
	for (int i = 1; i < numBytes; i++) {
		constant <<= 8;
		constant |= chunk->code[offset + i + 1];
	}
	printf("%-22s %5d\n", name, constant);
	return offset + 1 + numBytes;
}

static int getPropertyInstruction(const char *name, Chunk *chunk, int offset) {
	uint16_t constant = chunk->code[offset + 1];
	constant |= chunk->code[offset + 2];
	printf("%-22s %5d (", name, constant);
	printValue(chunk->constants.values[constant]);
	printf(")\n");
	return offset + 3;
}

static int invokeInstruction(const char *name, Chunk *chunk, int offset) {
	uint16_t constant = (uint16_t)(chunk->code[offset + 1] << 8);
	constant |= chunk->code[offset + 2];
	uint8_t argCount = chunk->code[offset + 3];
	printf("%-22s (%d args) %4d (", name, argCount, constant);
	printValue(chunk->constants.values[constant]);
	printf(")\n");
	return offset + 4;
}

static int memberInvokeInstruction(const char *name, Chunk *chunk, int offset) {
	uint16_t slot = (uint16_t)(chunk->code[offset + 1] << 8);
	slot |= chunk->code[offset + 2];
	uint8_t argCount = chunk->code[offset + 3];
	printf("%-22s (%d args) %u\n", name, argCount, slot);
	return offset + 4;
}

static int simpleInstruction(const char *name, int offset) {
	printf("%s\n", name);
	return offset + 1;
}

static int byteInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	printf("%-22s %4d\n", name, slot);
	return offset + 2;
}

static int shortInstruction(const char *name, Chunk *chunk, int offset) {
	uint16_t slot = (uint16_t)(chunk->code[offset + 1] << 8);
	slot |= chunk->code[offset + 2];
	printf("%-22s %5d\n", name, slot);
	return offset + 3;
}

static int jumpInstruction(const char *name, int sign, Chunk *chunk, int offset) {
	uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
	jump |= chunk->code[offset + 2];
	printf("%-22s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
	return offset + 3;
}

static int exceptionHandlerInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t stackLevel = chunk->code[offset + 1];
	uint16_t handlerData = (uint16_t)(chunk->code[offset + 2] << 8);
	handlerData |= chunk->code[offset + 3];
	printf("%-22s [%d] @%d\n", name, stackLevel, handlerData);
	return offset + 4;
}

static int arrayBuildInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t objType = chunk->code[offset + 1];
	uint16_t numItems = (uint16_t)(chunk->code[offset + 2] << 8);
	numItems |= chunk->code[offset + 3];
	const char *type = (objType == OBJ_ARRAY) ? "ARRAY" : "TUPLE";
	printf("%-22s %s [%d]\n", name, type, numItems);
	return offset + 4;
}

static int forEachInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t hasNextSlot = chunk->code[offset + 1];
	bool hasNextPostArgs = chunk->code[offset + 2];
	uint8_t nextSlot = chunk->code[offset + 3];
	bool nextPostArgs = chunk->code[offset + 4];
	printf("%-22s %4d %4d\n", name, hasNextSlot, nextSlot);
	return offset + 5;
}

static int unpackInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t numVal = chunk->code[offset + 1];
	bool first = true;
	printf("%-22s ", name);
	int argOffset = offset + 2;
	int argSize = 0;
	for (int i = 0; i < numVal; i++) {
		if (!first)
			printf(", ");
		first = false;

		VarType varType = chunk->code[argOffset];
		switch (varType) {
			case VAR_LOCAL:
				printf("L %d %s", chunk->code[argOffset + 1], chunk->code[argOffset + 1] ? "POST" : "PRE");
				argSize += 3;
				argOffset += 3;
				break;
			case VAR_UPVALUE:
				printf("U %d", chunk->code[argOffset + 1]);
				argSize += 2;
				argOffset += 2;
				break;
			case VAR_GLOBAL: {
				uint16_t slot = (uint16_t)(chunk->code[argOffset + 1] << 8);
				slot |= chunk->code[argOffset + 2];
				printf("G %d", slot);
				argSize += 3;
				argOffset += 3;
				break;
			}
		}
	}
	printf("\n");
	return offset + 1 + 1 + argSize;
}

static int resolveMembersInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t numMembers = chunk->code[offset + 1];
	printf("%-22s\n", name);

	for (int i = 0; i < numMembers; i++) {
		unsigned char *entry = chunk->code + offset + 2 + 5 * i;
		uint8_t type = entry[0];
		bool super = type & 0x1;
		uint8_t mask = (type & 0x6) >> 1;
		const char *strMask[] = {"field", "method", "any"};
		uint16_t nameIndex = (uint16_t)(entry[1] << 8);
		nameIndex |= entry[2];
		uint16_t slot = (uint16_t)(entry[3] << 8);
		slot |= entry[4];
		printf("        |                        [%u]=%s[%s %u (",
			   slot, super ? "super" : "this",
			   strMask[mask - 1], nameIndex);
		printValue(chunk->constants.values[nameIndex]);
		printf(")]\n");
	}

	return offset + 2 + 5 * numMembers;
}

static int dataInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t len = chunk->code[offset + 1];
	printf("%-22s [%d]=[", name, len);
	for (int i = 0; i < len; i++) {
		if (i == 0)
			printf("%d", chunk->code[offset + 2 + i]);
		else
			printf(",%d", chunk->code[offset + 2 + i]);
	}
	printf("]\n");
	return offset + 1 + len + 1;
}

static int localInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	uint8_t postArgs = chunk->code[offset + 2];
	printf("%-22s %5d %s\n", name, slot, postArgs ? "POST" : "PRE");
	return offset + 3;
}

int disassembleInstruction(Chunk *chunk, int offset) {
	printf("%04d ", offset);
	int line = getLine(chunk, offset);
	if (offset > 0 && line == getLine(chunk, offset - 1)) {
		printf("   | ");
	} else {
		printf("%4d ", line);
	}

	uint8_t instruction = chunk->code[offset];
	switch (instruction) {
		case OP_CONST8:
			return constantInstruction("CONST8", chunk, offset, 1);
		case OP_CONST16:
			return constantInstruction("CONST16", chunk, offset, 2);
		case OP_IMM8:
			return byteInstruction("IMM8", chunk, offset);
		case OP_IMM16:
			return shortInstruction("IMM16", chunk, offset);
		case OP_NIL:
			return simpleInstruction("NIL", offset);
		case OP_TRUE:
			return simpleInstruction("TRUE", offset);
		case OP_FALSE:
			return simpleInstruction("FALSE", offset);
		case OP_POP:
			return simpleInstruction("POP", offset);
		case OP_POPN:
			return byteInstruction("POPN", chunk, offset);
		case OP_NUM_VARARGS:
			return simpleInstruction("NUM_VARARGS", offset);
		case OP_GET_LOCAL:
			 return localInstruction("GET_LOCAL", chunk, offset);
		case OP_GET_VARARG:
			return simpleInstruction("GET_VARARG", offset);
		case OP_SET_LOCAL:
			return localInstruction("SET_LOCAL", chunk, offset);
		case OP_SET_VARARG:
			return simpleInstruction("SET_VARARG", offset);
		case OP_GET_GLOBAL:
			return globalInstruction("GET_GLOBAL", chunk, offset, 2);
		case OP_DEFINE_GLOBAL:
			return globalInstruction("DEFINE_GLOBAL", chunk, offset, 2);
		case OP_SET_GLOBAL:
			return globalInstruction("SET_GLOBAL", chunk, offset, 2);
		case OP_GET_UPVALUE:
			return byteInstruction("GET_UPVALUE", chunk, offset);
		case OP_SET_UPVALUE:
			return byteInstruction("SET_UPVALUE", chunk, offset);
		case OP_GET_PROPERTY:
			return getPropertyInstruction("GET_PROPERTY", chunk, offset);
		case OP_MAP_GET:
			return shortInstruction("MAP_GET", chunk, offset);
		case OP_GET_MEMBER_PROPERTY:
			return shortInstruction("GET_MEMBER_PROPERTY", chunk, offset);
		case OP_SET_PROPERTY:
			return constantInstruction("SET_PROPERTY", chunk, offset, 2);
		case OP_SET_MEMBER_PROPERTY:
			return shortInstruction("SET_MEMBER_PROPERTY", chunk, offset);
		case OP_MAP_SET:
			return constantInstruction("MAP_SET", chunk, offset, 2);
		case OP_GET_SUPER:
			return shortInstruction("GET_SUPER", chunk, offset);
		case OP_EQUAL:
			return simpleInstruction("EQUAL", offset);
		case OP_GREATER:
			return simpleInstruction("GREATER", offset);
		case OP_LESS:
			return simpleInstruction("LESS", offset);
		case OP_ADD:
			return simpleInstruction("ADD", offset);
		case OP_SUBTRACT:
			return simpleInstruction("SUBTRACT", offset);
		case OP_MULTIPLY:
			return simpleInstruction("MULTIPLY", offset);
		case OP_DIVIDE:
			return simpleInstruction("DIVIDE", offset);
		case OP_MODULO:
			return simpleInstruction("MODULO", offset);
		case OP_INSTANCEOF:
			return simpleInstruction("INSTANCEOF", offset);
		case OP_NOT:
			return simpleInstruction("NOT", offset);
		case OP_NEGATE:
			return simpleInstruction("NEGATE", offset);
		case OP_JUMP:
			return jumpInstruction("JUMP", 1, chunk, offset);
		case OP_JUMP_IF_FALSE:
			return jumpInstruction("JUMP_IF_FALSE", 1, chunk, offset);
		case OP_LOOP:
			return jumpInstruction("LOOP", -1, chunk, offset);
		case OP_CALL:
			return byteInstruction("CALL", chunk, offset);
		case OP_INVOKE:
			return invokeInstruction("INVOKE", chunk, offset);
		case OP_MEMBER_INVOKE:
			return memberInvokeInstruction("MEMBER_INVOKE", chunk, offset);
		case OP_SUPER_INVOKE:
			return invokeInstruction("SUPER_INVOKE", chunk, offset);
		case OP_SUPER_INIT:
			return byteInstruction("SUPER_INIT", chunk, offset);
		case OP_CLOSURE: {
			offset++;
			uint16_t constant = (uint16_t)(chunk->code[offset++] << 8);
			constant |= chunk->code[offset++];
			printf("%-22s %4d ", "CLOSURE", constant);
			printValue(chunk->constants.values[constant]);
			printf("\n");

			ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
			for (int j = 0; j < function->upvalueCount; j++) {
				int isLocal = chunk->code[offset++];
				int index = chunk->code[offset++];
				printf("%04d    |                     %s %d\n",
					   offset - 2, isLocal ? "local" : "upvalue", index);
			}

			return offset;
		}
		case OP_CLOSE_UPVALUE:
			return simpleInstruction("CLOSE_UPVALUE", offset);
		case OP_RETURN:
			return simpleInstruction("RETURN", offset);
		case OP_CLASS:
			return constantInstruction("CLASS", chunk, offset, 2);
		case OP_ANON_CLASS:
			return simpleInstruction("ANON_CLASS", offset);
		case OP_INHERIT:
			return simpleInstruction("INHERIT", offset);
		case OP_METHOD:
			return constantInstruction("METHOD", chunk, offset, 2);
		case OP_FIELD:
			return constantInstruction("FIELD", chunk, offset, 2);
		case OP_RESOLVE_MEMBERS:
			return resolveMembersInstruction("RESOLVE_MEMBERS", chunk, offset);
		case OP_ARRAY_BUILD:
			return arrayBuildInstruction("ARRAY_BUILD", chunk, offset);
		case OP_INDEX:
			return simpleInstruction("INDEX", offset);
		case OP_INDEX_STORE:
			return simpleInstruction("INDEX_STORE", offset);
		case OP_MAP_BUILD:
			return shortInstruction("MAP_BUILD", chunk, offset);
		case OP_THROW:
			return simpleInstruction("THROW", offset);
		case OP_PUSH_EXCEPTION_HANDLER:
			return exceptionHandlerInstruction("PUSH_EXCEPTION_HANDLER", chunk, offset);
		case OP_POP_EXCEPTION_HANDLER:
			return byteInstruction("POP_EXCEPTION_HANDLER", chunk, offset);
		case OP_FOREACH_INIT:
			return forEachInstruction("FOREACH_INIT", chunk, offset);
		case OP_UNPACK:
			return unpackInstruction("UNPACK", chunk, offset);
		case OP_IMPORT:
			return constantInstruction("IMPORT", chunk, offset, 2);
		case OP_DATA:
			return dataInstruction("DATA", chunk, offset);
		default:
			printf("Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}
