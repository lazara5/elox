#ifndef ELOX_OBJECT_H
#define ELOX_OBJECT_H

#include <stdarg.h>

#include "elox/common.h"
#include "elox/chunk.h"
#include "elox/table.h"
#include "elox/CloseTable.h"
#include "elox/function.h"
#include "elox/util.h"

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
#define AS_CSTRING(value)          (((ObjString *)AS_OBJ(value))->string.chars)
#define OBJ_AS_CSTRING(obj)        (((ObjString *)obj)->string.chars)
#define AS_STRINGPAIR(value)       ((ObjStringPair *)AS_OBJ(value))
#define OBJ_AS_STRINGPAIR(obj)     ((ObjStringPair *)obj)

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
	uint16_t arity;
	uint16_t maxArgs;
	uint16_t upvalueCount;
	Chunk chunk;
	ObjString *name;
	ObjClass *parentClass;
} ObjFunction;

typedef Value (*NativeFn)(Args *args);
typedef Value (*NativeClosureFn)(Args *args, int numUpvalues, Value *upvalues);

typedef struct {
	Obj obj;
	NativeFn function;
	uint16_t arity;
	uint16_t maxArgs;
} ObjNative;

typedef struct {
	Obj obj;
	NativeClosureFn function;
	uint16_t arity;
	uint16_t maxArgs;
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

typedef struct {
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

typedef Value *(*GetMemberRef)(RefData *refData, ObjInstance *instance);

typedef struct {
	GetMemberRef getMemberRef;
	RefData refData;
} MemberRef;

typedef struct ObjClass {
	Obj obj;
	size_t baseId;
	size_t classId;
	ObjString *name;
	Value initializer;
	Value hashCode;
	Value equals;
	Value super;
	Table fields;
	Table methods;
	Table statics;
	ValueArray staticValues;
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
	CloseTable items;
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
ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name,
						   NativeFn method, uint16_t arity, bool hasVarargs);
int addClassField(VMCtx *vmCtx, ObjClass *clazz, const char *name);

ObjString *takeString(VMCtx *vmCtx, char *chars, int length, int capacity);
ObjString *copyString(VMCtx *vmCtx, const char *chars, int length);

ObjStringPair *copyStrings(VMCtx *vmCtx,
						   const char *chars1, int len1, const char *chars2, int len2);

void initHeapString(VMCtx *vmCtx, HeapCString *str);
void initHeapStringWithSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity);

char *reserveHeapString(VMCtx *vmCtx, HeapCString *string, int len);
void heapStringAddString(VMCtx *vmCtx, HeapCString *string, const char *str, int len);
void heapStringAddChar(VMCtx *vmCtx, HeapCString *string, uint8_t ch);
void heapStringAddFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) ELOX_PRINTF(3, 4);
void heapStringAddVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap);

void freeHeapString(VMCtx *vmCtx, HeapCString *str);

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot);

ObjArray *newArray(VMCtx *vmCtx, int initialSize, ObjType objType);
void appendToArray(VMCtx *vmCtx, ObjArray *array, Value value);
bool isValidArrayIndex(ObjArray *array, int index);
Value arrayAt(ObjArray *array, int index);
Value arrayAtSafe(ObjArray *array, int index);
void arraySet(ObjArray *array, int index, Value value);

ObjMap *newMap(VMCtx *vmCtx);

void printValueObject(Value value);
void printObject(Obj *obj);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif // ELOX_OBJECT_H
