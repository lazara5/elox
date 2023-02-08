// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_VALUE_H
#define ELOX_VALUE_H

#include <elox/util.h>
#include <elox.h>

typedef struct VMCtx VMCtx;

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
	VAL_NUMBER = 0,
	VAL_NIL = 1,
	VAL_BOOL = 2,
	VAL_EXCEPTION = 3,
	VAL_UNDEFINED = 4,
	VAL_OBJ = 7
} ELOX_PACKED ValueType;

#ifdef ELOX_ENABLE_NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL       1 // 001
#define TAG_BOOL      2 // 010
#define TAG_EXCEPTION 3 // 011
#define TAG_UNDEFINED 4 // 100

#define TAG_MASK      7 // 111

typedef uint64_t Value;

#define BOOL_BIT 8 // 1000

#define IS_BOOL(value)      (((value) | BOOL_BIT) == TRUE_VAL)
#define IS_NIL(value)       ((value) == NIL_VAL)
#define IS_EXCEPTION(value) ((value) == EXCEPTION_VAL)
#define IS_UNDEFINED(value) ((value) == UNDEFINED_VAL)
#define IS_NUMBER(value)    (((value) & QNAN) != QNAN)
#define IS_OBJ(value) \
	(((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value)      ((value) == TRUE_VAL)
#define AS_NUMBER(value)    valueToNum(value)
#define AS_OBJ(value) \
	((Obj *)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_BOOL))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | BOOL_BIT | TAG_BOOL))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define EXCEPTION_VAL   ((Value)(uint64_t)(QNAN | TAG_EXCEPTION))
#define UNDEFINED_VAL   ((Value)(uint64_t)(QNAN | TAG_UNDEFINED))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj) \
	(Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double valueToNum(Value value) {
	double num;
	memcpy(&num, &value, sizeof(Value));
	return num;
}

static inline Value numToValue(double num) {
	Value value;
	memcpy(&value, &num, sizeof(double));
	return value;
}

#else

typedef struct {
	ValueType type;
	union {
		bool boolean;
		double number;
		Obj *obj;
	} as;
} Value;

#define IS_BOOL(value)      ((value).type == VAL_BOOL)
#define IS_NIL(value)       ((value).type == VAL_NIL)
#define IS_NUMBER(value)    ((value).type == VAL_NUMBER)
#define IS_EXCEPTION(value) ((value).type == VAL_EXCEPTION)
#define IS_UNDEFINED(value) ((value).type == VAL_UNDEFINED)
#define IS_OBJ(value)       ((value).type == VAL_OBJ)

#define AS_OBJ(value)       ((value).as.obj)
#define AS_BOOL(value)      ((value).as.boolean)
#define AS_NUMBER(value)    ((value).as.number)

#define BOOL_VAL(value)     ((Value){ VAL_BOOL, { .boolean = value } })
#define NIL_VAL             ((Value){ VAL_NIL, { .number = 0 } })
#define EXCEPTION_VAL       ((Value){ VAL_EXCEPTION, { .number = 0 } })
#define UNDEFINED_VAL       ((Value){ VAL_UNDEFINED, { .number = 0 } })
#define NUMBER_VAL(value)   ((Value){ VAL_NUMBER, { .number = value } })
#define OBJ_VAL(object)     ((Value){ VAL_OBJ, { .obj = (Obj *)object} })

#endif // ELOX_ENABLE_NAN_BOXING

typedef struct {
	int capacity;
	int count;
	Value *values;
} ValueArray;

void initValueArray(ValueArray *array);
void initSizedValueArray(VMCtx *vmCtx, ValueArray *array, size_t size);
void writeValueArray(VMCtx *vmCtx, ValueArray *array, Value value);
void freeValueArray(VMCtx *vmCtx, ValueArray *array);
void printValue(VMCtx *vmCtx, EloxIOStream stream, Value value);

typedef struct Error Error;

uint32_t hashValue(Value value, Error *error);
bool valuesEquals(const Value a, const Value b, Error *error);

#endif // ELOX_VALUE_H
