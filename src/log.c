/*
 * Log system.
 *
 * $Id: log.c,v 1.5 2004/08/15 05:19:39 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <time.h>

#include "log.h"
#include "parallel.h"

static int gdebug = 0;

static char *level_msg[] = {
	"info",
	"warning",
	"error",
	"debug"
};


void
ri_log_set_debug(int onoff)
{
	gdebug = onoff;
}

int
ri_log_get_debug()
{
	return gdebug;
}

void
ri_log(int level, const char *message, ...)
{
	va_list list;

	//time_t tm;

	va_start(list, message);

	assert(level < sizeof(level_msg));

	//time(&tm);

	fprintf(stdout, "[lucille] %s : ", level_msg[level]);
	vfprintf(stdout, message, list);
	fprintf(stdout, "\n");
	fflush(stdout);	// For VC binary running in cygwin console.
#if 0
		/* print only if master node */
		if (ri_parallel_taskid() == 0) {
			fprintf(stdout, "[%s] %s: %s, line %d | %s",
					level, message, file, line, ctime(&tm));
		}
#endif
}
