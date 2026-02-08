// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_TEMP_H
#define ELOX_TEMP_H

#include <elox/util.h>
#include <elox/value.h>

#define ELOX_TMP_ENUM(T) __tmp_ ## T,

#define TMP_SCOPE(FIBER, ...) \
	enum { FOR_EACH(ELOX_TMP_ENUM, __VA_ARGS__) __tmp_num }; \
	Value __ELOX_TMP_VALUES[__tmp_num] = { 0 }; \
	VMTempScope *__ELOX_OLD_TEMPS = (FIBER)->tempScopes; \
	VMTempScope **__ELOX_TMP_HEAD = &((FIBER)->tempScopes); \
	VMTempScope __LOCAL_TMP_SCOPE = { .vals = __ELOX_TMP_VALUES, .numVal =  __tmp_num, .next = (FIBER)->tempScopes }; \
	*__ELOX_TMP_HEAD = &__LOCAL_TMP_SCOPE

#define PUSH_TEMP(NAME, VAL) \
	__ELOX_TMP_VALUES[__tmp_ ## NAME] = (VAL)

#define RELEASE_TEMPS \
	*__ELOX_TMP_HEAD = __ELOX_OLD_TEMPS

#define TMP_SCOPE_PUSH(FIBER, VAL) \
	TMP_SCOPE(FIBER, __TEMP__); \
	PUSH_TEMP(__TEMP__, VAL)

#endif // ELOX_TEMP_H
