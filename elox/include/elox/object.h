#ifndef ELOX_OBJECT_H
#define ELOX_OBJECT_H

#include <stdarg.h>

#include "elox/common.h"
#include "elox/chunk.h"
#include "elox/table.h"
#include "elox/value.h"
#include "elox/valueTable.h"

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

#define AS_MAP(value)            ((ObjMap *)AS_OBJ(value))
#define AS_TUPLE(value)          ((ObjArray *)AS_OBJ(value))
#define AS_ARRAY(value)          ((ObjArray *)AS_OBJ(value))
#define AS_BOUND_METHOD(value)   ((ObjBoundMethod *)AS_OBJ(value))
#define AS_CLASS(value)          ((ObjClass *)AS_OBJ(value))
#define AS_CLOSURE(value)        ((ObjClosure *)AS_OBJ(value))
#define AS_NATIVE_CLOSURE(value) ((ObjNativeClosure *)AS_OBJ(value))
#define AS_FUNCTION(value)       ((ObjFunction *)AS_OBJ(value))
#define AS_INSTANCE(value)       ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value)         (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value)         ((ObjString *)AS_OBJ(value))
#define AS_STRINGPAIR(value)     ((ObjStringPair *)AS_OBJ(value))
#define AS_CSTRING(value)        (((ObjString *)AS_OBJ(value))->string.chars)

typedef enum {
	OBJ_BOUND_METHOD,
	OBJ_CLASS,
	OBJ_CLOSURE,
	OBJ_NATIVE_CLOSURE,
	OBJ_FUNCTION,
	OBJ_INSTANCE,
	OBJ_NATIVE,
	OBJ_STRING,
	OBJ_STRINGPAIR,
	OBJ_UPVALUE,
	OBJ_ARRAY,
	OBJ_TUPLE,
	OBJ_MAP
} ObjType;

struct Obj {
	ObjType type;
	bool isMarked;
	struct Obj *next;
};

typedef struct ObjClass ObjClass;

typedef struct {
	Obj obj;
	int arity;
	int upvalueCount;
	Chunk chunk;
	ObjString *name;
	ObjClass *parentClass;
} ObjFunction;

typedef Value (*NativeFn)(VMCtx *vmCtx, int argCount, Value *args);
typedef Value (*NativeClosureFn)(VMCtx *vmCtx, int argCount, Value *args,
								 int numUpvalues, Value *upvalues);

typedef struct {
	Obj obj;
	NativeFn function;
} ObjNative;

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

typedef struct {
	Obj obj;
	ObjFunction *function;
	ObjUpvalue **upvalues;
	int upvalueCount;
} ObjClosure;

typedef struct {
	Obj obj;
	NativeClosureFn nativeFunction;
	int upvalueCount;
	Value *upvalues;
} ObjNativeClosure;

typedef struct ObjInstance ObjInstance;

typedef union {
	Value *value;
	intptr_t offset;
} RefData;

typedef Value *(*GetMemberRef)(RefData *refData, ObjInstance *instance);

typedef struct {
	GetMemberRef getMemberRef;
	RefData refData;
} MemberRef;

typedef struct ObjClass {
	Obj obj;
	ObjString *name;
	Value initializer;
	Value hashCode;
	Value equals;
	Value super;
	Table fields;
	Table methods;
	MemberRef *memberRefs;
	int memberRefCount;
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

typedef struct {
	Obj obj;
	Value receiver;
	Obj *method;
} ObjBoundMethod;

typedef struct {
	Obj obj;
	int size;
	int capacity;
	Value *items;
} ObjArray;

typedef struct {
	Obj obj;
	ValueTable items;
} ObjMap;

typedef struct {
	char *chars;
	int length;
	int capacity;
} HeapCString;

static inline uint32_t hashString(const char *key, int length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

ObjBoundMethod *newBoundMethod(VMCtx *vmCtx, Value receiver, Obj *method);
ObjClass *newClass(VMCtx *vmCtx, ObjString *name);

ObjClosure *newClosure(VMCtx *vmCtx, ObjFunction *function);

ObjNativeClosure *newNativeClosure(VMCtx *vmCtx, NativeClosureFn function, uint8_t numUpvalues);

ObjFunction *newFunction(VMCtx *vmCtx);
ObjInstance *newInstance(VMCtx *vmCtx, ObjClass *clazz);
ObjNative *newNative(VMCtx *vmCtx, NativeFn function);
ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name, NativeFn method);
void addClassField(VMCtx *vmCtx, ObjClass *clazz, const char *name);

ObjString *takeString(VMCtx *vmCtx, char *chars, int length, int capacity);
ObjString *copyString(VMCtx *vmCtx, const char *chars, int length);

ObjStringPair *copyStrings(VMCtx *vmCtx,
						   const char *chars1, int len1, const char *chars2, int len2);

void initHeapString(VMCtx *vmCtx, HeapCString *str);
void initHeapStringWithSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity);

char *reserveHeapString(VMCtx *vmCtx, HeapCString *string, int len);
void addHeapString(VMCtx *vmCtx, HeapCString *string, const char *str, int len);
void addHeapStringFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) ELOX_PRINTF(3, 4);
void addHeapStringVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap);

void freeHeapString(VMCtx *vmCtx, HeapCString *str);

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot);

ObjArray *newArray(VMCtx *vmCtx, int initialSize, ObjType objType);
void appendToArray(VMCtx *vmCtx, ObjArray *array, Value value);
bool isValidArrayIndex(ObjArray *array, int index);
Value arrayAt(ObjArray *array, int index);
Value arrayAtSafe(ObjArray *array, int index);
void arraySet(ObjArray *array, int index, Value value);

ObjMap *newMap(VMCtx *vmCtx);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif // ELOX_OBJECT_H
