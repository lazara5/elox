// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_OBJECT_H
#define ELOX_OBJECT_H

#include <stdarg.h>

#include <elox/value.h>
#include <elox/chunk.h>
#include <elox/ValueArray.h>
#include <elox/util.h>
#include "elox/ValueTable.h"
#include "elox/function.h"
#include "elox/elox-config-internal.h"

typedef EloxString String;

#define IS_KLASS(value)          (isObjType(value, OBJ_INTERFACE) || isObjType(value, OBJ_CLASS))
#define IS_STRING(value)         isObjType(value, OBJ_STRING)

#define AS_STRING(value)         ((ObjString *)AS_OBJ(value))
#define OBJ_AS_STRING(obj)       ((ObjString *)obj)
#define AS_CSTRING(value)        ((const char *)((ObjString *)AS_OBJ(value))->string.chars)
#define OBJ_AS_CSTRING(obj)      (((ObjString *)obj)->string.chars)

// Keep within 8 bits !
typedef enum {
#define ELOX_OBJTAGS_INLINE
#define INITTAG(name, init) OBJ_##name = init,
#define TAG(name) OBJ_##name,
#include "objTags.h"
#undef TAG
#undef INITTAG
#undef ELOX_OBJTAGS_INLINE
} ELOX_PACKED ObjType;

Obj *allocateObject(RunCtx *runCtx, size_t size, ObjType type);

#define ALLOCATE_OBJ(runCtx, type, objectType) \
	(type *)allocateObject(runCtx, sizeof(type), objectType)

static const uint8_t MARKER_BLACK = 1 << 0;
static const uint8_t MARKER_GRAY =  1 << 1;

struct Obj {
#ifdef ELOX_SUPPORTS_PACKED
	ObjType type;
#else
	ObjType type: 8;
#endif
	uint8_t markers;
	struct Obj *next;
};

typedef struct ObjClass ObjClass;

typedef struct ObjFunction {
	Obj obj;
	bool isMethod;
	uint16_t arity;
	uint16_t maxArgs;
	uint16_t upvalueCount;
	uint16_t refOffset;
	Chunk chunk;
	ObjString *name;
	ObjClass *parentClass;
	Value *defaultArgs;
} ObjFunction;

typedef Value (*NativeFn)(Args *args);
typedef Value (*NativeClosureFn)(Args *args, int numUpvalues, Value *upvalues);

typedef struct {
	Obj obj;
	NativeFn function;
	uint16_t arity;
	uint16_t maxArgs;
	Value *defaultArgs;
} ObjNative;

typedef struct {
	Obj obj;
	NativeClosureFn function;
	uint16_t arity;
	uint16_t maxArgs;
	Value *defaultArgs;
	uint16_t upvalueCount;
	Value *upvalues;
} ObjNativeClosure;

typedef struct ObjString {
	Obj obj;
	String string;
	uint32_t hash;
} ObjString;

typedef struct ObjStringPair {
	Obj obj;
	ObjString *str1;
	ObjString *str2;
	uint32_t hash;
} ObjStringPair;

typedef struct ObjUpvalue {
	Obj obj;
	Value *location;
	Value closed;
	struct ObjUpvalue *next;
} ObjUpvalue;

typedef struct ObjClosure {
	Obj obj;
	ObjFunction *function;
	ObjUpvalue **upvalues;
	int upvalueCount;
} ObjClosure;

typedef struct {
	Obj obj;
	int32_t size;
	int32_t capacity;
	uint32_t modCount;
	Value *items;
} ObjArray;

typedef struct {
	Obj obj;
	ValueTable items;
} ObjHashMap;

typedef struct {
	uint8_t *chars;
	int length;
	int capacity;
} HeapCString;

typedef struct ObjCallFrame {
	Obj obj; // Not actually heap-allocated !
	ObjClosure *closure;
	ObjFunction *function;
	uint8_t *ip;
	Value *slots;
	FrameType type : 8;
	uint8_t fixedArgs;
	uint8_t varArgs;
	uint8_t argOffset;
	uint16_t stackArgs; // for native call frames only
	uint8_t tryDepth;
	TryBlock *tryStack;
} ObjCallFrame;

static inline uint32_t hashString(const uint8_t *key, int length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

ObjClosure *newClosure(RunCtx *runCtx, ObjFunction *function);

ObjNativeClosure *newNativeClosure(RunCtx *runCtx, NativeClosureFn function,
								   uint16_t arity, uint8_t numUpvalues);

typedef enum {
	FTYPE_FUNCTION,
	FTYPE_INITIALIZER,
	FTYPE_METHOD,
	FTYPE_DEFAULT,
	FTYPE_LAMBDA,
	FTYPE_SCRIPT
} FunctionType;

ObjFunction *newFunction(RunCtx *runCtx, ObjString *fileName);
ObjNative *newNative(RunCtx *vmCtx, NativeFn function, uint16_t arity);
ObjString *internString(RunCtx *runCtx, const uint8_t *chars, int32_t length, EloxError *error);

ObjString *takeString(RunCtx *runCtx, uint8_t *chars, int length, int capacity);
ObjString *copyString(RunCtx *runCtx, const uint8_t *chars, int32_t length);

ObjStringPair *copyStrings(RunCtx *runCtx,
						   const uint8_t *chars1, int len1, const uint8_t *chars2, int len2);

bool initHeapString(RunCtx *runCtx, HeapCString *str);
bool initHeapStringWithSize(RunCtx *runCtx, HeapCString *str, int initialCapacity);

uint8_t *reserveHeapString(RunCtx *runCtx, HeapCString *string, int len);
bool heapStringAddString(RunCtx *runCtx, HeapCString *string, const uint8_t *str, int len);
bool heapStringAddChar(RunCtx *runCtx, HeapCString *string, uint8_t ch);
bool heapStringAddFmt(RunCtx *runCtx, HeapCString *string, const char *format, ...) ELOX_PRINTF(3, 4);
bool heapStringAddVFmt(RunCtx *runCtx, HeapCString *string, const char *format, va_list ap);

void freeHeapString(RunCtx *runCtx, HeapCString *str);

ObjUpvalue *newUpvalue(RunCtx *runCtx, Value *slot);

ObjArray *newArray(RunCtx *runCtx, int initialSize, ObjType objType);
bool appendToArray(RunCtx *runCtx, ObjArray *array, Value value);
bool isValidArrayIndex(ObjArray *array, int index);
Value arrayAt(ObjArray *array, int index);
Value arrayAtSafe(RunCtx *runCtx, ObjArray *array, int32_t index);
void arraySet(ObjArray *array, int index, Value value);
Value arraySlice(RunCtx *runCtx, ObjArray *array, ObjType type, Value start, Value end);
bool arrayContains(RunCtx *runCtx, ObjArray *seq, const Value needle, EloxError *error);

ObjHashMap *newHashMap(RunCtx *runCtx);

void printValueObject(RunCtx *runCtx, EloxIOStream stream, Value value);
void printObject(RunCtx *runCtx, EloxIOStream stream, Obj *obj);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

typedef enum {
	VTYPE_BOOL = VAL_BOOL,
	VTYPE_NIL = VAL_NIL,
	VTYPE_NUMBER = VAL_NUMBER,
	VTYPE_EXCEPTION = VAL_EXCEPTION,
	VTYPE_UNDEFINED = VAL_UNDEFINED,
	VTYPE_OBJ_STRING = OBJ_STRING,
	VTYPE_OBJ_BOUND_METHOD = OBJ_BOUND_METHOD,
	VTYPE_OBJ_INTERFACE = OBJ_INTERFACE,
	VTYPE_OBJ_CLASS = OBJ_CLASS,
	VTYPE_OBJ_CLOSURE = OBJ_CLOSURE,
	VTYPE_OBJ_NATIVE_CLOSURE = OBJ_NATIVE_CLOSURE,
	VTYPE_OBJ_FUNCTION = OBJ_FUNCTION,
	VTYPE_OBJ_INSTANCE = OBJ_INSTANCE,
	VTYPE_OBJ_NATIVE = OBJ_NATIVE,
	VTYPE_OBJ_STRINGPAIR = OBJ_STRINGPAIR,
	VTYPE_OBJ_UPVALUE = OBJ_UPVALUE,
	VTYPE_OBJ_ARRAY = OBJ_ARRAY,
	VTYPE_OBJ_TUPLE = OBJ_TUPLE,
	VTYPE_OBJ_HASHMAP = OBJ_HASHMAP,
	VTYPE_OBJ_FRAME = OBJ_FRAME,
	VTYPE_MAX
} ELOX_PACKED ValueTypeId;

#ifdef ELOX_ENABLE_NAN_BOXING
static ValueTypeId valueTypeId(Value val) {
	if (IS_NUMBER(val))
		return VTYPE_NUMBER;
	if (IS_OBJ(val))
		return (ValueTypeId)AS_OBJ(val)->type;
	return (val & TAG_MASK) >> TAGSHIFT;
}
#else
static ValueTypeId valueTypeId(Value val) {
	return val.type == VAL_OBJ
		? (ValueTypeId)(val.as.obj->type)
		: (ValueTypeId)val.type;
}
#endif // ELOX_ENABLE_NAN_BOXING

ELOX_FORCE_INLINE
static bool computeSlice(Value start, Value end, int32_t size,
						 int32_t *sliceStart, int32_t *sliceEnd) {
	ValueTypeId startType = valueTypeId(start);
	ValueTypeId endType = valueTypeId(end);

	if (startType == VTYPE_NIL)
		*sliceStart = 0;
	else if (ELOX_LIKELY(startType == VTYPE_NUMBER))
		*sliceStart = AS_NUMBER(start);
	else
		return false;

	if (endType == VTYPE_NIL)
		*sliceEnd = size;
	else if (ELOX_LIKELY(endType == VTYPE_NUMBER))
		*sliceEnd = AS_NUMBER(end);
	else
		return false;

	if (*sliceStart < 0)
		*sliceStart = 0;
	else if (*sliceStart > size)
		*sliceStart = size;

	if (*sliceEnd < 0)
		*sliceEnd = 0;

	if (*sliceEnd < *sliceStart)
		*sliceEnd = *sliceStart;
	else if (*sliceStart > size)
		*sliceEnd = size;

	return true;
}

#endif // ELOX_OBJECT_H
