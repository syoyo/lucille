/*
 * Dynamic library loading routine.
 */
#include <stdio.h>
#include <stdlib.h>

#if defined(WIN32)
#include <windows.h>
#elif defined(LINUX) || (defined(__APPLE__) && defined(__MACH__))
/* Mac OS X Panther(10.3) now supports dlopen API. */
#include <dlfcn.h>
#endif

#include "dlload.h"
#include "hash.h"
#include "memory.h"
#include "log.h"

static ri_hash_t *module_cache;

dl_module_t *
dlload(const char *filename)
{
	static int   initialized = 0;
	dl_module_t *module      = NULL;
	char         msg[1024];

	if (!initialized) {
		module_cache = ri_hash_new();
		initialized = 1;
	}

	if (ri_hash_lookup(module_cache, filename)) {
		sprintf(msg, "Hit shader module cache [ %s ]\n", filename);
		ri_log(LOG_INFO, msg);

		/* hit module cache! */
		module = (dl_module_t *)ri_hash_lookup(module_cache, filename);

		return module;
	}

	module = (dl_module_t *)ri_mem_alloc(sizeof(dl_module_t));

#if defined(WIN32)
	module->module = LoadLibrary(filename);
	if (module->module == NULL) {
		/* Can't load module library */
		ri_mem_free(module);
		return NULL;
	}

#elif defined(LINUX) || (defined(__APPLE__) && defined(__MACH__))
	module->module = dlopen(filename, RTLD_LAZY);
	if (module->module == NULL) {
		/* Can't load module library */
		ri_mem_free(module);
		return NULL;
	}
#endif

	/* add to module cache */
	ri_hash_insert(module_cache, filename, module);

	return module;
}

void *
dlgetfunc(dl_module_t *module, const char *funcname)
{
	void *func = NULL;

#if defined(WIN32)
	func = GetProcAddress(module->module, funcname);
#elif defined (LINUX) || (defined(__APPLE__) && defined(__MACH__))
	const char *error;

	func = dlsym(module->module, funcname);
	error = dlerror();
	if (error) {
		printf("can't find symbol: %s\n", error);
		return NULL;
	}
#endif

	return func;
}

