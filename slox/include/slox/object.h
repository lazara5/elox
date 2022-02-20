#ifndef SLOX_OBJECT_H
#define SLOX_OBJECT_H

#include <stdarg.h>

#include "slox/common.h"
#include "slox/chunk.h"
#include "slox/table.h"
#include "slox/value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_ARRAY(value)        isObjType(value, OBJ_ARRAY)
#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value)        isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)     isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

#define AS_ARRAY(value)        ((ObjArray *)AS_OBJ(value))
#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)AS_OBJ(value))
#define AS_CLASS(value)        ((ObjClass *)AS_OBJ(value))
#define AS_CLOSURE(value)      ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFunction *)AS_OBJ(value))
#define AS_INSTANCE(value)     ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value)       (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value)       ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString *)AS_OBJ(value))->chars)

typedef enum {
	OBJ_BOUND_METHOD,
	OBJ_CLASS,
	OBJ_CLOSURE,
	OBJ_FUNCTION,
	OBJ_INSTANCE,
	OBJ_NATIVE,
	OBJ_STRING,
	OBJ_UPVALUE,
	OBJ_ARRAY
} ObjType;

struct Obj {
	ObjType type;
	bool isMarked;
	struct Obj *next;
};

typedef struct {
	Obj obj;
	int arity;
	int upvalueCount;
	Chunk chunk;
	ObjString *name;
} ObjFunction;

typedef Value (*NativeFn)(VMCtx *vmCtx, int argCount, Value *args);

typedef struct {
	Obj obj;
	NativeFn function;
} ObjNative;

struct ObjString {
	Obj obj;
	int length;
	char *chars;
	uint32_t hash;
};

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
	ObjString *name;
	Table methods;
} ObjClass;

typedef struct {
	Obj obj;
	ObjClass *clazz;
	uint32_t identityHash;
	Table fields;
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
	char *chars;
	int length;
	int capacity;
} HeapCString;

ObjBoundMethod *newBoundMethod(VMCtx *vmCtx, Value receiver, Obj *method);
ObjClass *newClass(VMCtx *vmCtx, ObjString *name);
ObjClosure *newClosure(VMCtx *vmCtx, ObjFunction *function);
ObjFunction *newFunction(VMCtx *vmCtx);
ObjInstance *newInstance(VMCtx *vmCtx, ObjClass *clazz);
ObjNative *newNative(VMCtx *vmCtx, NativeFn function);
ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name, NativeFn method);

ObjString *takeString(VMCtx *vmCtx, char *chars, int length, int capacity);
ObjString *copyString(VMCtx *vmCtx, const char *chars, int length);

void initHeapString(VMCtx *vmCtx, HeapCString *str);
void initHeapStringSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity);

void addStringFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) SLOX_PRINTF(3, 4);
void addStringVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap);

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot);

ObjArray *newArray(VMCtx *vmCtx);
void appendToArray(VMCtx *vmCtx, ObjArray *array, Value value);
bool isValidArrayIndex(ObjArray *array, int index);
Value arrayAt(ObjArray *array, int index);
void arraySet(ObjArray *array, int index, Value value);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif // SLOX_OBJECT_H
