#include <elox/function.h>

void setValueArg(Args *args, int i, Value val) {
	args->frame->slots[i] = val;
}

Value getValueArg(Args *args, int i) {
	return args->frame->slots[i];
}
