// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/function.h>
#include <elox/object.h>

void setValueArg(Args *args, int i, Value val) {
	args->frame->slots[i] = val;
}

Value getValueArg(Args *args, int i) {
	return args->frame->slots[i];
}

ValueArray getArgsFrom(Args *args, int i) {
	bool overflow = i >= args->count;
	uint16_t count = overflow ? 0 : args->count - i;
	return (ValueArray){ .count = count, .values = overflow ? NULL : args->frame->slots + i };
}
