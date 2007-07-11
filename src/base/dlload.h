/*
 * Dynamic Library loader.
 *
 * $Id: dlload.h,v 1.2 2004/01/23 02:46:07 syoyo Exp $
 */
#ifndef DLLOAD_H
#define DLLOAD_H

#if defined(WIN32)
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _dl_module_t
{
#if defined(WIN32)
	HINSTANCE         module;
#elif defined(LINUX) || (defined(__APPLE__) && defined(__MACH__))
	void             *module;
#else
	int               module;
#endif
} dl_module_t;

extern dl_module_t *dlload(const char *filename);
extern void        *dlgetfunc(dl_module_t *module, const char *funcname);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

