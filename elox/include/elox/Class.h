#ifndef ELOX_CLASS_H
#define ELOX_CLASS_H

#include <elox/object.h>
#include <elox/table.h>
#include <elox/StringIntTable.h>

typedef struct ObjKlass {
	// preamble to ObjInterface and ObjClass
	Obj obj;

	uint8_t typeCheckOffset;
	ObjString *name;
} ObjKlass;

typedef struct ObjInterface {
// [ Klass
	Obj obj;
	uint8_t typeCheckOffset;
	ObjString *name;
//   Klass ]
	Table methods;
} ObjInterface;

typedef struct {
	// +1 for cache
	Obj *rptDisplay[ELOX_CLASS_DISPLAY_SIZE + 1];
	Obj **rssList;
	uint8_t depth;
	uint16_t numRss;
} TypeInfo;

typedef struct ObjMethod ObjMethod;

typedef struct ObjClass {
// [ Klass
	Obj obj;
	uint8_t typeCheckOffset;
	ObjString *name;
//   Klass ]
	TypeInfo typeInfo;
	Value initializer;
	ObjMethod *hashCode;
	ObjMethod *equals;
	Value super;
	StringIntTable fields;
	Table methods;
	Table statics;
	ValueArray staticValues;
	MemberRef *memberRefs;
	uint16_t memberRefCount;
	bool abstract;
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
	uint16_t arity;
	bool hasVarargs;
} ObjMethodDesc;

ObjInterface *newInterface(RunCtx *runCtx, ObjString *name);
ObjClass *newClass(RunCtx *runCtx, ObjString *name, bool abstract);

ObjBoundMethod *newBoundMethod(RunCtx *runCtx, Value receiver, ObjMethod *method);
ObjMethod *newMethod(RunCtx *runCtx, ObjClass *clazz, Obj *callable);
ObjMethodDesc *newMethodDesc(RunCtx *runCtx, uint8_t arity, bool hasVarargs);
void addMethod(RunCtx *runCtx, ObjInterface *intf, ObjString *methodName,
			   uint16_t arity, bool hasVarargs, EloxError *error);
ObjNative *addNativeMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *methodName,
						   NativeFn method, uint16_t arity, bool hasVarargs, EloxError *error);
int addClassField(RunCtx *runCtx, ObjClass *clazz, ObjString *fieldName, EloxError *error);


#endif //ELOX_CLASS_H
