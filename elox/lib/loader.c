// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <stdio.h>
#include <sys/stat.h>
#include <time.h>
#include <stdlib.h>

#include <elox/elox-internal.h>
#include <elox/elox-config-internal.h>
#include <elox/value.h>
#include <elox/builtins/string.h>

static Value isReadableFile(VMCtx *vmCtx, const String *name, const String *pattern, Error *error) {
	VM *vm = &vmCtx->vm;

DBG_PRINT_STACK("IRF0", vmCtx);
	push(vm, OBJ_VAL(newNative(vmCtx, stringGsub, 4)));
	push(vm, OBJ_VAL(copyString(vmCtx, pattern->chars, pattern->length)));
	push(vm, OBJ_VAL(copyString(vmCtx, ELOX_USTR_AND_LEN("?"))));
	push(vm, OBJ_VAL(copyString(vmCtx, name->chars, name->length)));
	Value fileName = runCall(vmCtx, 3);
DBG_PRINT_STACK("IRF1", vmCtx);
	if (ELOX_UNLIKELY(IS_EXCEPTION(fileName))) {
		error->raised = true;
		return NIL_VAL;
	}
	pop(vm);

	const char *strFileName = AS_CSTRING(fileName);

	struct stat path_stat;
	if (stat(strFileName, &path_stat) != 0)
		return NIL_VAL;

	if (!S_ISREG(path_stat.st_mode))
		return NIL_VAL;

	FILE *f = fopen(strFileName, "r");  // try to open file
	if (f == NULL)
		return NIL_VAL;
	fclose(f);
	return fileName;
}

typedef enum {
	PATH_SCAN,
	PATH_TOKEN,
	PATH_WS
} PathState;

static Value searchPath(const String *name, const char *path, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	const uint8_t *start = (const uint8_t *)path;
	const uint8_t *end = start;
	PathState state = PATH_SCAN;

	while (*end != '\0') {
		uint8_t c = *end;
		switch (state) {
			case PATH_SCAN:
				if (isWhitespace(c)) {
					// skip leading whitespace
				} else if (c == ';') {
					// don't care about empty tokens
				} else {
					start = end;
					state = PATH_TOKEN;
				}
				break;
			case PATH_TOKEN:
				if (isWhitespace(c))
					state = PATH_WS;
				else if (c == ';') {
					String pattern = { .chars = start, .length = end - start };
					Value fileName = isReadableFile(vmCtx, name, &pattern, error);
					ELOX_IF_RAISED_RET_VAL(error, NIL_VAL);
					if (!IS_NIL(fileName))
						return fileName;

					state = PATH_SCAN;
				}
				break;
			case PATH_WS:
				if (isWhitespace(c)) {
					//skip whitespace
				} else if (c == ';') {
					String pattern = { .chars = start, .length = end - start };
					Value fileName = isReadableFile(vmCtx, name, &pattern, error);
					ELOX_IF_RAISED_RET_VAL(error, NIL_VAL);
					if (!IS_NIL(fileName))
						return fileName;

					state = PATH_SCAN;
				} else
					state = PATH_TOKEN;
				break;
		}
		end++;
	}

	// final token
	switch (state) {
		case PATH_SCAN:
		case PATH_WS:
			// ignore whitespace
			break;
		case PATH_TOKEN: {
			String pattern = { .chars = start, .length = end - start };
			Value fileName = isReadableFile(vmCtx, name, &pattern, error);
			ELOX_IF_RAISED_RET_VAL(error, NIL_VAL);
			if (!IS_NIL(fileName))
				return fileName;

			break;
		}
	}

	return NIL_VAL;
}

// --- sys library --------------------------------------------------------

static String eloxBuiltinSysModule = STRING_INITIALIZER("sys");

static Value clockNative(Args *args ELOX_UNUSED) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value loadBuiltinSysModule(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	const String clockName = STRING_INITIALIZER("clock");
	registerNativeFunction(vmCtx, &clockName, &eloxBuiltinSysModule, clockNative, 0, false);

	return NIL_VAL;
}

Value eloxBuiltinModuleLoader(const String *moduleName, uint64_t options, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	if (stringEquals(moduleName, &eloxBuiltinSysModule)) {
		return ((options & ELOX_BML_ENABLE_SYS) == 0) ? NIL_VAL:
				OBJ_VAL(newNative(vmCtx, loadBuiltinSysModule, 0));
	}

	return NIL_VAL;
}

static uint8_t *loadFile(VMCtx *vmCtx, const char *path) {
	FILE *file = fopen(path, "rb");
	if (file == NULL) {
		runtimeError(vmCtx, "Could not open file '%s'", path);
		return NULL;
	}

	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	rewind(file);

	uint8_t *buffer = ALLOCATE(vmCtx, uint8_t, fileSize + 1);
	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
	if (bytesRead < fileSize) {
		FREE(vmCtx, char, buffer);
		fclose(file);
		runtimeError(vmCtx, "Could not read file '%s'", path);
		return NULL;
	}

	buffer[bytesRead] = '\0';

	fclose(file);
	return buffer;
}

Value eloxFileModuleLoader(const String *moduleName, uint64_t options ELOX_UNUSED, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	const char *modulePath = getenv("ELOX_LIBRARY_PATH");
	if (modulePath == NULL)
		modulePath = "?.elox";

	Value moduleFile = searchPath(moduleName, modulePath, error);
	if (ELOX_UNLIKELY(error->raised))
		return NIL_VAL;
	if (IS_NIL(moduleFile))
		return NIL_VAL;

	pushTemp(vmCtx, moduleFile);
	ObjString *fileName = AS_STRING(moduleFile);

	Value ret = NIL_VAL;
	uint8_t *source = loadFile(vmCtx, (const char *)fileName->string.chars);
	if (ELOX_UNLIKELY(source == NULL))
		goto cleanup;

	ObjFunction *function = compile(vmCtx, source, moduleName);
	if (function == NULL) {
		runtimeError(vmCtx, "Could not compile module '%s'", moduleName->chars);
		goto cleanup;
	}
	ret = OBJ_VAL(function);

cleanup:
	if (source != NULL)
		FREE(vmCtx, char, source);
	popTemp(vmCtx); // moduleFile

	return ret;
}
