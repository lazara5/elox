// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_OBJECT_H
#define ELOX_OBJECT_H

#include <stdarg.h>

#include <elox/value.h>
#include <elox/chunk.h>
#include <elox/ValueArray.h>
#include "elox/util.h"
#include "elox/chunk.h"
#include "elox/table.h"
#include "elox/ValueTable.h"
#include "elox/function.h"

#define OBJ_TYPE(value)          (AS_OBJ(value)->type)

#define IS_MAP(value)            isObjType(value, OBJ_MAP)
#define IS_TUPLE(value)          isObjType(value, OBJ_TUPLE)
#define IS_ARRAY(value)          isObjType(value, OBJ_ARRAY)
#define IS_BOUND_METHOD(value)   isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value)          isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value)        isObjType(value, OBJ_CLOSURE)
#define IS_NATIVE_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)       isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)       isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)         isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)         isObjType(value, OBJ_STRING)

#define AS_MAP(value)              ((ObjMap *)AS_OBJ(value))
#define OBJ_AS_MAP(obj)            ((ObjMap *)obj)
#define AS_TUPLE(value)            ((ObjArray *)AS_OBJ(value))
#define OBJ_AS_TUPLE(obj)          ((ObjArray *)obj)
#define AS_ARRAY(value)            ((ObjArray *)AS_OBJ(value))
#define OBJ_AS_ARRAY(obk)          ((ObjArray *)obj)
#define AS_BOUND_METHOD(value)     ((ObjBoundMethod *)AS_OBJ(value))
#define OBJ_AS_BOUND_METHOD(obj)   ((ObjBoundMethod *)obj)
#define AS_METHOD(value)           ((ObjMethod *)AS_OBJ(value))
#define OBJ_AS_METHOD(obj)         ((ObjMethod *)obj)
#define AS_CLASS(value)            ((ObjClass *)AS_OBJ(value))
#define OBJ_AS_CLASS(obj)          ((ObjClass *)obj)
#define AS_CLOSURE(value)          ((ObjClosure *)AS_OBJ(value))
#define OBJ_AS_CLOSURE(obj)        ((ObjClosure *)obj)
#define AS_NATIVE_CLOSURE(value)   ((ObjNativeClosure *)AS_OBJ(value))
#define OBJ_AS_NATIVE_CLOSURE(obj) ((ObjNativeClosure *)obj)
#define AS_FUNCTION(value)         ((ObjFunction *)AS_OBJ(value))
#define OBJ_AS_FUNCTION(obj)       ((ObjFunction *)obj)
#define AS_INSTANCE(value)         ((ObjInstance *)AS_OBJ(value))
#define OBJ_AS_INSTANCE(obj)       ((ObjInstance *)obj)
#define AS_NATIVE(value)           (((ObjNative *)AS_OBJ(value)))
#define OBJ_AS_NATIVE(obj)         (((ObjNative *)obj))
#define AS_STRING(value)           ((ObjString *)AS_OBJ(value))
#define OBJ_AS_STRING(obj)         ((ObjString *)obj)
#define AS_CSTRING(value)          ((const char *)((ObjString *)AS_OBJ(value))->string.chars)
#define OBJ_AS_CSTRING(obj)        (((ObjString *)obj)->string.chars)
#define AS_STRINGPAIR(value)       ((ObjStringPair *)AS_OBJ(value))
#define OBJ_AS_STRINGPAIR(obj)     ((ObjStringPair *)obj)

typedef enum {
	OBJ_STRING,
	OBJ_BOUND_METHOD,
	OBJ_METHOD,
	OBJ_CLASS,
	OBJ_CLOSURE,
	OBJ_NATIVE_CLOSURE,
	OBJ_FUNCTION,
	OBJ_INSTANCE,
	OBJ_NATIVE,
	OBJ_STRINGPAIR,
	OBJ_UPVALUE,
	OBJ_ARRAY,
	OBJ_TUPLE,
	OBJ_MAP
} ELOX_PACKED ObjType;

static const uint8_t MARKER_BLACK = 1 << 0;
static const uint8_t MARKER_TEMP =   1 << 1;

struct Obj {
	ObjType type;
	uint8_t markers;
	struct Obj *next;
};

typedef struct ObjClass ObjClass;

typedef struct {
	Obj obj;
	uint16_t arity;
	uint16_t maxArgs;
	uint16_t upvalueCount;
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

typedef struct ObjInstance ObjInstance;

typedef union {
	Value *value;
	intptr_t offset;
} RefData;

typedef enum {
	REFTYPE_CLASS_MEMBER,
	REF_TYPE_INST_FIELD
} ELOX_PACKED RefType;

typedef struct {
	RefType refType;
	union {
		uint32_t propIndex;
		Value *value;
	} data;
} MemberRef;

typedef struct ObjMethod ObjMethod;

typedef struct ObjClass {
	Obj obj;
	size_t baseId;
	size_t classId;
	ObjString *name;
	Value initializer;
	ObjMethod *hashCode;
	ObjMethod *equals;
	Value super;
	Table fields;
	Table methods;
	Table statics;
	ValueArray staticValues;
	MemberRef *memberRefs;
	uint16_t memberRefCount;
} ObjClass;

#define INST_HAS_HASHCODE (1UL << 0)
#define INST_HAS_EQUALS   (1UL << 1)

typedef struct ObjInstance {
	Obj obj;
	ObjClass *clazz;
	uint32_t identityHash;
	uint8_t flags;
	ValueArray fields;
} ObjInstance;

typedef struct ObjBoundMethod {
	Obj obj;
	Value receiver;
	Obj *method;
} ObjBoundMethod;

typedef struct ObjMethod {
	Obj obj;
	ObjClass *clazz;
	Obj *callable;
} ObjMethod;

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
} ObjMap;

typedef struct {
	uint8_t *chars;
	int length;
	int capacity;
} HeapCString;

static inline uint32_t hashString(const uint8_t *key, int length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

typedef EloxErrorMsg ErrorMsg;

ObjBoundMethod *newBoundMethod(VMCtx *vmCtx, Value receiver, ObjMethod *method);
ObjMethod *newMethod(VMCtx *vmCtx, ObjClass *clazz, Obj *callable);
ObjClass *newClass(VMCtx *vmCtx, ObjString *name);

ObjClosure *newClosure(VMCtx *vmCtx, ObjFunction *function);

ObjNativeClosure *newNativeClosure(VMCtx *vmCtx, NativeClosureFn function,
								   uint16_t arity, uint8_t numUpvalues);

ObjFunction *newFunction(VMCtx *vmCtx);
ObjInstance *newInstance(VMCtx *vmCtx, ObjClass *clazz);
ObjNative *newNative(VMCtx *vmCtx, NativeFn function, uint16_t arity);
ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name,
						   NativeFn method, uint16_t arity, bool hasVarargs, ErrorMsg *errorMsg);
int addClassField(VMCtx *vmCtx, ObjClass *clazz, const char *name, ErrorMsg *error);

ObjString *takeString(VMCtx *vmCtx, uint8_t *chars, int length, int capacity);
ObjString *copyString(VMCtx *vmCtx, const uint8_t *chars, int32_t length);

ObjStringPair *copyStrings(VMCtx *vmCtx,
						   const uint8_t *chars1, int len1, const uint8_t *chars2, int len2);

bool initHeapString(VMCtx *vmCtx, HeapCString *str);
bool initHeapStringWithSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity);

uint8_t *reserveHeapString(VMCtx *vmCtx, HeapCString *string, int len);
bool heapStringAddString(VMCtx *vmCtx, HeapCString *string, const uint8_t *str, int len);
bool heapStringAddChar(VMCtx *vmCtx, HeapCString *string, uint8_t ch);
bool heapStringAddFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) ELOX_PRINTF(3, 4);
bool heapStringAddVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap);

void freeHeapString(VMCtx *vmCtx, HeapCString *str);

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot);

ObjArray *newArray(VMCtx *vmCtx, int initialSize, ObjType objType);
bool appendToArray(VMCtx *vmCtx, ObjArray *array, Value value);
bool isValidArrayIndex(ObjArray *array, int index);
Value arrayAt(ObjArray *array, int index);
Value arrayAtSafe(VMCtx *vmCtx, ObjArray *array, int32_t index);
void arraySet(ObjArray *array, int index, Value value);
Value arraySlice(VMCtx *vmCtx, ObjArray *array, ObjType type, Value start, Value end);
bool arrayContains(ObjArray *seq, const Value needle, Error *error);

ObjMap *newMap(VMCtx *vmCtx);

void printValueObject(VMCtx *vmCtx, EloxIOStream stream, Value value);
void printObject(VMCtx *vmCtx, EloxIOStream stream, Obj *obj);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

typedef enum {
	VTYPE_BOOL = VAL_BOOL,
	VTYPE_NIL = VAL_NIL,
	VTYPE_NUMBER = VAL_NUMBER,
	VTYPE_EXCEPTION = VAL_EXCEPTION,
	VTYPE_UNDEFINED = VAL_UNDEFINED,
	VTYPE_OBJ_STRING = VAL_OBJ + OBJ_STRING,
	VTYPE_OBJ_BOUND_METHOD = VAL_OBJ + OBJ_BOUND_METHOD,
	VTYPE_OBJ_CLASS = VAL_OBJ + OBJ_CLASS,
	VTYPE_OBJ_CLOSURE = VAL_OBJ + OBJ_CLOSURE,
	VTYPE_OBJ_NATIVE_CLOSURE = VAL_OBJ + OBJ_NATIVE_CLOSURE,
	VTYPE_OBJ_FUNCTION = VAL_OBJ + OBJ_FUNCTION,
	VTYPE_OBJ_INSTANCE = VAL_OBJ + OBJ_INSTANCE,
	VTYPE_OBJ_NATIVE = VAL_OBJ + OBJ_NATIVE,
	VTYPE_OBJ_STRINGPAIR = VAL_OBJ + OBJ_STRINGPAIR,
	VTYPE_OBJ_UPVALUE = VAL_OBJ + OBJ_UPVALUE,
	VTYPE_OBJ_ARRAY = VAL_OBJ + OBJ_ARRAY,
	VTYPE_OBJ_TUPLE = VAL_OBJ + OBJ_TUPLE,
	VTYPE_OBJ_MAP = VAL_OBJ + OBJ_MAP,
	VTYPE_MAX
} ELOX_PACKED ValueTypeId;

#ifdef ELOX_ENABLE_NAN_BOXING
static ValueTypeId valueTypeId(Value val) {
	if (IS_NUMBER(val))
		return VTYPE_NUMBER;
	if (IS_OBJ(val))
		return VAL_OBJ + AS_OBJ(val)->type;
	return val & TAG_MASK;
}
#else
static ValueTypeId valueTypeId(Value val) {
	return val.type == VAL_OBJ
		? (ValueTypeId)(VAL_OBJ + val.as.obj->type)
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
