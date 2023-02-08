// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/function.h>

void setValueArg(Args *args, int i, Value val) {
	args->frame->slots[i] = val;
}

Value getValueArg(Args *args, int i) {
	return args->frame->slots[i];
}
