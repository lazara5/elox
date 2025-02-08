// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_CLASS_H
#define ELOX_CLASS_H

#include <elox/object.h>
#include <elox/table.h>
#include <elox/PropTable.h>

typedef enum {
	ELOX_DT_SUPER,
	ELOX_DT_CLASS,
	ELOX_DT_INST,
	ELOX_DT_NUM
} ELOX_PACKED DataTable;

typedef struct {
#ifdef ELOX_SUPPORTS_PACKED
	DataTable tableIndex;
	bool isMethod;
#else
	DataTable tableIndex : 8;
#endif
	uint16_t propIndex;
} Ref;

// preamble to ObjKlass and ObjInstance
typedef struct {
	Obj obj;

	Value *tables[ELOX_DT_NUM];
} ObjData;

// preamble to ObjInterface and ObjClass
typedef struct ObjKlass {
// [ Data
	Obj obj;
	Value *tables[ELOX_DT_NUM];
//   Data ]
	uint8_t typeCheckOffset;
	ObjString *name;
} ObjKlass;

typedef struct ObjInterface {
// [ Klass
	Obj obj;
	Value *tables[ELOX_DT_NUM];
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
	Value *tables[ELOX_DT_NUM];
	uint8_t typeCheckOffset;
	ObjString *name;
//   Klass ]
	TypeInfo typeInfo;
	Value initializer;
	ObjMethod *hashCode;
	ObjMethod *equals;
	Value super;
	PropTable props;
	uint32_t numFields;
	ValueArray classData;
	Ref *refs;
	uint16_t numRefs;
	bool abstract;
} ObjClass;

#define INST_HAS_HASHCODE (1UL << 0)
#define INST_HAS_EQUALS   (1UL << 1)

typedef struct ObjInstance {
// [ Data
	Obj obj;
	Value *tables[ELOX_DT_NUM];
//   Data ]
	ObjClass *clazz;
	uint32_t identityHash;
	uint8_t flags;
	uint16_t numFields;
	Value *fields;
} ObjInstance;

static inline bool pushClassData(RunCtx *runCtx, ObjClass *clazz, Value value) {
	bool pushed = valueArrayPush(runCtx, &clazz->classData, value);
	if (ELOX_LIKELY(pushed))
		clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
	return pushed;
}

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
ObjInstance *newInstance(RunCtx *runCtx, ObjClass *clazz);
ObjBoundMethod *newBoundMethod(RunCtx *runCtx, Value receiver, ObjMethod *method);
ObjMethod *newMethod(RunCtx *runCtx, ObjClass *clazz, Obj *callable);
ObjMethodDesc *newMethodDesc(RunCtx *runCtx, uint8_t arity, bool hasVarargs);
void addMethod(RunCtx *runCtx, ObjInterface *intf, ObjString *methodName,
			   uint16_t arity, bool hasVarargs, EloxError *error);
ObjNative *addNativeMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *methodName,
						   NativeFn method, uint16_t arity, bool hasVarargs, EloxError *error);
int addClassField(RunCtx *runCtx, ObjClass *clazz, ObjString *fieldName, EloxError *error);


#endif //ELOX_CLASS_H
