// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/elox-config-internal.h>

#if defined(ELOX_CONFIG_WIN32)
#include <windows.h>
#endif // ELOX_CONFIG_WIN32

#include <stdio.h>
#include <sys/stat.h>
#include <time.h>
#include <stdlib.h>

#include <elox/elox-internal.h>
#include <elox/value.h>
#include <elox/builtins/string.h>

#if defined(ELOX_CONFIG_WIN32)
	#ifndef _S_ISTYPE
	#define _S_ISTYPE(mode, mask)  (((mode) & _S_IFMT) == (mask))
	#define S_ISREG(mode) _S_ISTYPE((mode), _S_IFREG)
	#endif

	#define MODULE_EXT ".dll"
#else
	#define MODULE_EXT ".so"
#endif // ELOX_CONFIG_WIN32

static Value isReadableFile(VMCtx *vmCtx, const String *name, const String *pattern, Error *error) {
	VM *vm = &vmCtx->vm;

	push(vm, OBJ_VAL(vm->builtins.stringGsub));
	ObjString *patternStr = copyString(vmCtx, pattern->chars, pattern->length);
	ELOX_COND_RAISE_RET_VAL((patternStr == NULL), error, OOM(), NIL_VAL);
	push(vm, OBJ_VAL(patternStr));
	ObjString *qStr = copyString(vmCtx, ELOX_USTR_AND_LEN("?"));
	ELOX_COND_RAISE_RET_VAL((qStr == NULL), error, OOM(), NIL_VAL);
	push(vm, OBJ_VAL(qStr));
	ObjString *nameStr = copyString(vmCtx, name->chars, name->length);
	ELOX_COND_RAISE_RET_VAL((nameStr == NULL), error, OOM(), NIL_VAL);
	push(vm, OBJ_VAL(nameStr));
	Value fileName = runCall(vmCtx, 3);
	if (ELOX_UNLIKELY(IS_EXCEPTION(fileName))) {
		error->raised = true;
		return NIL_VAL;
	}
	pop(vm);

	const char *strFileName = AS_CSTRING(fileName);

	Value ret = NIL_VAL;

	FILE *f = fopen(strFileName, "r");  // try to open file
	if (f == NULL)
		goto cleanup;

	int fd = fileno(f);
	if (fd < 0)
		goto cleanup;

	struct stat pathStat;
	if (fstat(fd, &pathStat) < 0)
		goto cleanup;

	if (!S_ISREG(pathStat.st_mode))
		goto cleanup;

	ret = fileName;

cleanup:
	if (f != NULL)
		fclose(f);

	return ret;
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
	ObjNative *moduleFn = registerNativeFunction(vmCtx, &clockName,
												 &eloxBuiltinSysModule, clockNative, 0, false);
	if (ELOX_UNLIKELY(moduleFn == NULL))
		return oomError(vmCtx);

	return NIL_VAL;
}

Value eloxBuiltinModuleLoader(const String *moduleName, uint64_t options, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	if (stringEquals(moduleName, &eloxBuiltinSysModule)) {
		if ((options & ELOX_BML_ENABLE_SYS) == 0)
			return NIL_VAL;
		ObjNative *loader = newNative(vmCtx, loadBuiltinSysModule, 0);
		if (ELOX_UNLIKELY(loader == NULL)) {
			oomError(vmCtx);
			error->raised = true;
			return NIL_VAL;
		}
		return OBJ_VAL(loader);
	}

	return NIL_VAL;
}

static uint8_t *loadFile(const char *path, Error *error) {
	VMCtx *vmCtx = error->vmCtx;

	uint8_t *ret = NULL;
	uint8_t *buffer = NULL;

	FILE *file = fopen(path, "rb");
	ELOX_COND_RAISE_GOTO((file == NULL), error,
						 RTERR("Could not open file '%s'", path), cleanup);

	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	rewind(file);

	buffer = ALLOCATE(vmCtx, uint8_t, fileSize + 1);
	ELOX_COND_RAISE_GOTO((buffer == NULL), error, OOM(), cleanup);
	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
	ELOX_COND_RAISE_GOTO((bytesRead < fileSize), error,
						 RTERR("Could not read file '%s'", path), cleanup);

	buffer[bytesRead] = '\0';

	ret = buffer;
	buffer = NULL;

cleanup:
	if (buffer != NULL)
		FREE(vmCtx, char, buffer);

	if (file != NULL)
		fclose(file);

	return ret;
}

Value eloxFileModuleLoader(const String *moduleName, uint64_t options ELOX_UNUSED, Error *error) {
	VMCtx *vmCtx = error->vmCtx;
	VM *vm = &vmCtx->vm;

	const char *modulePath = getenv("ELOX_LIBRARY_PATH");
	if (modulePath == NULL)
		modulePath = "?.elox";

	Value moduleFile = searchPath(moduleName, modulePath, error);
	if (ELOX_UNLIKELY(error->raised))
		return NIL_VAL;
	if (IS_NIL(moduleFile))
		return NIL_VAL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(vm);
	PUSH_TEMP(temps, protectedFile, moduleFile);
	ObjString *fileName = AS_STRING(moduleFile);

	Value ret = NIL_VAL;
	uint8_t *source = loadFile((const char *)fileName->string.chars, error);
	if (ELOX_UNLIKELY(error->raised))
		goto cleanup;

	ObjFunction *function = compile(vmCtx, source, moduleName);
	if (function == NULL) {
		runtimeError(vmCtx, "Could not compile module '%s'", moduleName->chars);
		error->raised = true;
		goto cleanup;
	}
	ret = OBJ_VAL(function);

cleanup:
	if (source != NULL)
		FREE(vmCtx, char, source);
	releaseTemps(&temps);

	return ret;
}

#if defined(ELOX_CONFIG_WIN32)

static const char *getError(char *buffer, size_t buffer_size) {
	int error = GetLastError();
	if (!FormatMessageA(FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_FROM_SYSTEM,
						NULL, error, 0, buffer, buffer_size/sizeof(char), NULL))
	snprintf(buffer, buffer_size, "system error %d\n", error);
	return buffer;
}

static void *eloxDlopen(VMCtx *vmCtx, const char *path, Error *error) {
	HMODULE lib = LoadLibraryExA(path, NULL, 0);
	if (lib == NULL) {
		char buffer[128];
		error->raised = true;
		runtimeError(vmCtx, "LoadLibraryExA failed: %s", getError(buffer, sizeof(buffer)));
	}
	return lib;
}

static NativeFn eloxDlfcn(VMCtx *vmCtx, void *lib, const char *symName, Error *error) {
	NativeFn f = (NativeFn)GetProcAddress((HMODULE)lib, symName);
	if (f == NULL) {
		char buffer[128];
		error->raised = true;
		runtimeError(vmCtx, "GetProcAddress failed: %s", getError(buffer, sizeof(buffer)));
	}
	return f;
}

static void eloxDlclose(void *lib) {
	FreeLibrary((HMODULE)lib);
}

#else

#include <dlfcn.h>

static void *eloxDlopen(VMCtx *vmCtx, const char *path, Error *error) {
	void *lib = dlopen(path, RTLD_NOW | RTLD_LOCAL);
	if (lib == NULL) {
		error->raised = true;
		runtimeError(vmCtx, "dlopen failed: %s", dlerror());
	}
	return lib;
}

static NativeFn eloxDlfcn(VMCtx *vmCtx, void *lib, const char *symName, Error *error) {
	NativeFn f = dlsym(lib, symName);
	if (f == NULL) {
		error->raised = true;
		runtimeError(vmCtx, "dlopen failed: %s", dlerror());
	}
	return f;
}

static void eloxDlclose(void *lib) {
	dlclose(lib);
}

#endif // ELOX_CONFIG_WIN32

static const String NATIVE_LOADER_PREFIX = STRING_INITIALIZER("eloxLoad");

Value eloxNativeModuleLoader(const String *moduleName, uint64_t options ELOX_UNUSED, Error *error) {
	VMCtx *vmCtx = error->vmCtx;
	VM *vm = &vmCtx->vm;

	const char *modulePath = getenv("ELOX_NATIVE_LIBRARY_PATH");
	if (modulePath == NULL)
		modulePath = "?" MODULE_EXT;

	Value moduleFile = searchPath(moduleName, modulePath, error);
	if (ELOX_UNLIKELY(error->raised))
		return NIL_VAL;
	if (IS_NIL(moduleFile))
		return NIL_VAL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(vm);
	PUSH_TEMP(temps, protectedFile, moduleFile);
	const char *fileName = AS_CSTRING(moduleFile);

	Value ret = NIL_VAL;

	void *lib = eloxDlopen(vmCtx, fileName, error);
	if (lib == NULL)
		goto cleanup;
	char initFnName[256];
	snprintf(initFnName, sizeof(initFnName), "%s%s",
			 (const char *)NATIVE_LOADER_PREFIX.chars, (const char *)moduleName->chars);
	initFnName[NATIVE_LOADER_PREFIX.length] = upperLookup[(uint8_t)initFnName[NATIVE_LOADER_PREFIX.length]];

	NativeFn loadFn = eloxDlfcn(vmCtx, lib, initFnName, error);
	if (loadFn == NULL)
		goto cleanup;

	ObjNative *native = newNative(vmCtx, loadFn, 0);
	if (ELOX_UNLIKELY(native == NULL)) {
		oomError(vmCtx);
		error->raised = true;
		goto cleanup;
	}
	lib = NULL;
	ret = OBJ_VAL(native);

cleanup:
	if (lib != NULL)
		eloxDlclose(lib);
	releaseTemps(&temps);

	return ret;
}
