#include "elox/common.h"

#include <string.h>

bool stringEquals(const String *a, const String *b) {
	if (a->length != b->length)
		return false;
	return (memcmp(a->chars, b->chars, a->length) == 0);
}

