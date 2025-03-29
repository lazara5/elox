// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_CLASS_H
#define ELOX_CLASS_H

#include <elox/object.h>
#include <elox/table.h>
#include <elox/PropTable.h>
#include <elox/state.h>

typedef enum {
	ELOX_DT_SUPER,
	ELOX_DT_CLASS,
	ELOX_DT_INST,
	ELOX_DT_NUM
} ELOX_PACKED DataTable;

static const char* DataTableNames[] = {
	"SUPER",
	"CLASS",
	"INST"
};

typedef struct {
#ifdef ELOX_SUPPORTS_PACKED
	DataTable tableIndex;
	bool isMethod;
#else
	DataTable tableIndex : 8;
#endif
	uint16_t propIndex;
} Ref;

typedef enum {
	REF_THIS,
	REF_SUPER
} RefTarget;

#define MEMBER_FIELD_MASK  0x40000000
#define MEMBER_METHOD_MASK 0x80000000
#define MEMBER_ANY_MASK    0xC0000000

static inline uint8_t getRefSlotType(uint32_t slot, bool isSuper) {
	uint32_t memberType = (slot & MEMBER_ANY_MASK) >> 30;
	return (uint8_t)isSuper | memberType << 1;
}

// preamble to ObjKlass and ObjInstance
typedef struct {
	Obj obj;

	Value *tables[ELOX_DT_NUM];
} ObjData;

typedef struct ObjKlass ObjKlass;

typedef struct OpenKlass {
	ObjKlass *klass;

	RunCtx *runCtx;
	EloxError *error;

	CCtx cCtx;
	KlassCompiler klassCompiler;

	Table pendingThis;
	Table pendingSuper;
	uint32_t numRefs;
} OpenKlass;

OpenKlass *newOpenKlass(RunCtx *runCtx, ObjKlass *klass);
void freeOpenKlass(RunCtx *runCtx, OpenKlass *ok);

// preamble to ObjInterface and ObjClass
typedef struct ObjKlass {
// [ Data
	Obj obj;
	Value *tables[ELOX_DT_NUM];
//   Data ]
	uint8_t typeCheckOffset;
	ObjString *name;
	OpenKlass *openKlass;
} ObjKlass;

typedef struct ObjInterface {
// [ Klass
	Obj obj;
	Value *tables[ELOX_DT_NUM];
	uint8_t typeCheckOffset;
	ObjString *name;
	OpenKlass *openKlass;
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
	OpenKlass *openKlass;
//   Klass ]
	TypeInfo typeInfo;
	Value initializer;
	ObjMethod *hashCode;
	ObjMethod *equals;
	ObjClass *super;
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

static inline int pushClassData(RunCtx *runCtx, ObjClass *clazz, Value value) {
	bool pushed = valueArrayPush(runCtx, &clazz->classData, value);
	if (ELOX_LIKELY(pushed))
		clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
	return pushed ? (int)clazz->classData.count - 1 : -1;
}

static inline int setClassData(RunCtx *runCtx, ObjClass *clazz, unsigned int index, Value value)
{
	bool set = valueArraySet(runCtx, &clazz->classData, index, value);
	if (ELOX_LIKELY(set))
		clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
	return set ? (int)index : -1;
}

typedef struct ObjBoundMethod {
	Obj obj;
	Value receiver;
	Obj *method;
} ObjBoundMethod;

typedef struct ObjDefaultMethod ObjDefaultMethod;

typedef struct Prototype {
	uint16_t arity;
	bool hasVarargs;
} Prototype;

typedef struct ObjMethod {
	Obj obj;
	union {
		struct {
			ObjKlass *klass;
			Obj *callable;
			ObjDefaultMethod *fromDefault;
		} method;
		struct {
			ObjDefaultMethod *fromDefault;
		} pending;
		struct {
			Prototype proto;
		} abstract;
	};
	bool isConflicted;
} ObjMethod;

typedef struct {
	uint32_t offset;
	uint8_t slotType;
	uint16_t nameHandle;
} RefBindDesc;

typedef struct ObjDefaultMethod {
	Obj obj;
	ObjFunction *function;
	RefBindDesc *refs;
	uint16_t numRefs;
} ObjDefaultMethod;

ObjInterface *newInterface(RunCtx *runCtx, ObjString *name);
ObjClass *newClass(RunCtx *runCtx, ObjString *name, bool abstract);
ObjInstance *newInstance(RunCtx *runCtx, ObjClass *clazz);
ObjBoundMethod *newBoundMethod(RunCtx *runCtx, Value receiver, ObjMethod *method);
ObjMethod *newMethod(RunCtx *runCtx, ObjKlass *klass, Obj *callable);
ObjMethod *newPendingMethod(RunCtx *runCtx, ObjDefaultMethod *defaultMethod);
ObjDefaultMethod *newDefaultMethod(RunCtx *runCtx, ObjFunction *function);
ObjMethod *newAbstractMethod(RunCtx *runCtx, uint8_t arity, bool hasVarargs);
void addAbstractMethod(RunCtx *runCtx, Obj *parent, ObjString *methodName,
					   uint16_t arity, bool hasVarargs, EloxError *error);
ObjNative *addNativeMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *methodName,
						   NativeFn method, uint16_t arity, bool hasVarargs, EloxError *error);
int addClassField(RunCtx *runCtx, ObjClass *clazz, ObjString *fieldName, EloxError *error);

/*void resolveRef(RunCtx *runCtx, ObjClass *clazz, uint8_t slotType, ObjString *propName,
				uint16_t slot, EloxError *error);*/


void closeOpenKlass(RunCtx *runCtx, ObjKlass *klass, EloxError *error);
ObjNative *klassAddNativeMethod(EloxKlassHandle *okh, ObjString *methodName, NativeFn method,
								uint16_t arity, bool hasVarargs);
void klassAddAbstractMethod(EloxKlassHandle *okh, ObjString *methodName,
							uint16_t arity, bool hasVarargs);
int klassAddField(EloxKlassHandle *okh, ObjString *fieldName);
ObjMethod *klassAddCompiledMethod(EloxKlassHandle *okh, uint8_t *src, String *fileName, String *moduleName);
ObjKlass *klassClose(OpenKlass *oc);

/*#define OPEN_CLASS(NAME, RUNCTX, ABSTRACT, CLASSNAME, FILENAME, MODULENAME, ERROR, ...) \
	OpenKlass NAME = { .runCtx = RUNCTX, .error = ERROR, \
					   .fileName = FILENAME, .moduleName = MODULENAME }; \
	initOpenKlass(&NAME, RUNCTX, \
				  (ObjKlass *)registerGlobalClass(RUNCTX, ABSTRACT, CLASSNAME, MODULENAME, ERROR, __VA_ARGS__, NULL))*/

#endif //ELOX_CLASS_H
