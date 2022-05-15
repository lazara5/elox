#ifndef SLOX_VALUE_H
#define SLOX_VALUE_H

#include <string.h>

#include "common.h"

typedef struct VMCtx VMCtx;

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
	VAL_BOOL,
	VAL_NIL,
	VAL_NUMBER,
	VAL_EXCEPTION,
	VAL_UNDEFINED,
	VAL_OBJ
} ValueType;

#ifdef ENABLE_NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL       1 // 001
#define TAG_FALSE     2 // 010
#define TAG_TRUE      3 // 011
#define TAG_EXCEPTION 4 // 100
#define TAG_UNDEFINED 5 // 101

typedef uint64_t Value;

#define IS_BOOL(value)      (((value) | 1) == TRUE_VAL)
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
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
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

#endif // ENABLE_NAN_BOXING

typedef struct {
	int capacity;
	int count;
	Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void initSizedValueArray(VMCtx *vmCtx, ValueArray *array, size_t size);
void writeValueArray(VMCtx *vmCtx, ValueArray *array, Value value);
void freeValueArray(VMCtx *vmCtx, ValueArray *array);
void printValue(Value value);

#endif // SLOX_VALUE_H
