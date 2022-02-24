#include <stdio.h>

#include "slox/debug.h"
#include "slox/object.h"

void disassembleChunk(Chunk *chunk, const char *name) {
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count; ) {
		offset = disassembleInstruction(chunk, offset);
	}
}

static int constantInstruction(const char *name, Chunk *chunk, int offset) {
	uint16_t constant = chunk->code[offset + 1];
	constant |= chunk->code[offset + 2];
	printf("%-16s %5d <", name, constant);
	printValue(chunk->constants.values[constant]);
	printf(">\n");
	return offset + 3;
}

static int invokeInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t constant = chunk->code[offset + 1];
	uint8_t argCount = chunk->code[offset + 2];
	printf("%-16s (%d args) %4d <", name, argCount, constant);
	printValue(chunk->constants.values[constant]);
	printf(">\n");
	return offset + 3;
}

static int simpleInstruction(const char *name, int offset) {
	printf("%s\n", name);
	return offset + 1;
}

static int byteInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	printf("%-16s %4d\n", name, slot);
	return offset + 2;
}

static int shortInstruction(const char *name, Chunk *chunk, int offset) {
	uint16_t slot = chunk->code[offset + 1];
	slot |= chunk->code[offset + 2];
	printf("%-16s %d\n", name, slot);
	return offset + 3;
}

static int jumpInstruction(const char *name, int sign, Chunk *chunk, int offset) {
	uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
	jump |= chunk->code[offset + 2];
	printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
	return offset + 3;
}

static int exceptionHandlerInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t stackLevel = chunk->code[offset + 1];
	uint16_t handlerData = (uint16_t)(chunk->code[offset + 2] << 8);
	handlerData |= chunk->code[offset + 3];
	printf("%-16s [%d] @%d\n", name, stackLevel, handlerData);
	return offset + 4;
}

static int dataInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t len = chunk->code[offset + 1];
	printf("%-16s [%d]=[", name, len);
	for (int i = 0; i < len; i++) {
		if (i == 0)
			printf("%d", chunk->code[offset + 2 + i]);
		else
			printf(",%d", chunk->code[offset + 2 + i]);
	}
	printf("]\n");
	return offset + 1 + len + 1;
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
		case OP_CONSTANT:
			return constantInstruction("CONSTANT", chunk, offset);
		case OP_NIL:
			return simpleInstruction("NIL", offset);
		case OP_TRUE:
			return simpleInstruction("TRUE", offset);
		case OP_FALSE:
			return simpleInstruction("FALSE", offset);
		case OP_POP:
			return simpleInstruction("POP", offset);
		case OP_GET_LOCAL:
			 return byteInstruction("GET_LOCAL", chunk, offset);
		case OP_SET_LOCAL:
			 return byteInstruction("SET_LOCAL", chunk, offset);
		case OP_GET_GLOBAL:
			return constantInstruction("GET_GLOBAL", chunk, offset);
		case OP_DEFINE_GLOBAL:
			return constantInstruction("DEFINE_GLOBAL", chunk, offset);
		case OP_SET_GLOBAL:
			return constantInstruction("SET_GLOBAL", chunk, offset);
		case OP_GET_UPVALUE:
			return byteInstruction("GET_UPVALUE", chunk, offset);
		case OP_SET_UPVALUE:
			return byteInstruction("SET_UPVALUE", chunk, offset);
		case OP_GET_PROPERTY:
			return constantInstruction("GET_PROPERTY", chunk, offset);
		case OP_SET_PROPERTY:
			return constantInstruction("SET_PROPERTY", chunk, offset);
		case OP_GET_SUPER:
			return constantInstruction("GET_SUPER", chunk, offset);
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
		case OP_NOT:
			return simpleInstruction("NOT", offset);
		case OP_NEGATE:
			return simpleInstruction("NEGATE", offset);
		case OP_PRINT:
			return simpleInstruction("PRINT", offset);
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
		case OP_SUPER_INVOKE:
			return invokeInstruction("SUPER_INVOKE", chunk, offset);
		case OP_CLOSURE: {
			offset++;
			uint8_t constant = chunk->code[offset++];
			printf("%-16s %4d ", "CLOSURE", constant);
			printValue(chunk->constants.values[constant]);
			printf("\n");

			ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
			for (int j = 0; j < function->upvalueCount; j++) {
				int isLocal = chunk->code[offset++];
				int index = chunk->code[offset++];
				printf("%04d      |                     %s %d\n",
					   offset - 2, isLocal ? "local" : "upvalue", index);
			}

			return offset;
		}
		case OP_CLOSE_UPVALUE:
			return simpleInstruction("CLOSE_UPVALUE", offset);
		case OP_RETURN:
			return simpleInstruction("RETURN", offset);
		case OP_CLASS:
			return constantInstruction("CLASS", chunk, offset);
		case OP_INHERIT:
			return simpleInstruction("INHERIT", offset);
		case OP_METHOD:
			return constantInstruction("METHOD", chunk, offset);
		case OP_ARRAY_BUILD:
			return shortInstruction("ARRAY_BUILD", chunk, offset);
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
		case OP_DATA:
			return dataInstruction("DATA", chunk, offset);
		default:
			printf("Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}
