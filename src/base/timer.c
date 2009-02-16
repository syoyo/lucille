/*
 * Timer implementaion.
 *
 * $Id: timer.c,v 1.6 2004/06/13 06:44:51 syoyo Exp $
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WIN32
#include <windows.h>
#endif
#if defined(LINUX) || defined(__MACH__) || defined(SPARC) || defined(SGI) || defined(__FreeBSD__)
#include <sys/time.h>
#endif

#include "timer.h"
#include "memory.h"
#include "log.h"

typedef struct _timerinfo_t
{
#ifndef WIN32
	struct timeval start;
	struct timeval end;
#else
	LARGE_INTEGER start;
	LARGE_INTEGER end;
	LARGE_INTEGER freq;
#endif	
	char         *name;
	unsigned int  count;
	double        elapsed;
	int           starting;
} timerinfo_t;

static void   set_start_time   (timerinfo_t *timer);
static void   set_end_time     (timerinfo_t *timer);
static double get_elapsed_time_from_start(timerinfo_t *timer);
static double calc_elapsed_time(timerinfo_t *timer);
static void   timerinfo_free_func(void *data, void *userdata);
static void   timerinfo_dump_func(void *data, void *userdata);


/*
 * Function: ri_timer_new
 *
 *      Creates new timer structure. ri_timer_t records various timings with
 *      hasing by its tagname.
 *      Actual timing data is stored in timerinfo_t.
 *
 *  Parameters:
 *
 *      None.
 * 
 *  Returns:
 *
 *      The pointer to newly allocated memory for ri_timer_t.
 *
 */
ri_timer_t *
ri_timer_new()
{
	ri_timer_t *p = NULL;

	p = (ri_timer_t *)ri_mem_alloc(sizeof(ri_timer_t));

	p->entrylist = ri_hash_new();

	return p;
}

/*
 * Function: ri_timer_free
 *
 *     Frees memory for ri_timer_t variable.
 *
 * Parameters:
 *
 *     *timer - The pointer to ri_timer_t structure to be freed.
 *
 * Returns:
 *
 *     None.
 *
 */
void
ri_timer_free(ri_timer_t *timer)
{
	ri_hash_traverse(timer->entrylist, timerinfo_free_func, NULL);
	ri_mem_free(timer);
}

/*
 * Function: ri_timer_start
 *
 *     Starts time recoding specified by *name*.
 *     If the name specified by *name* doesn't been found in *timer*,
 *     create new timerinfo_t named *name* into *timer*.
 *
 * Parameters:
 *
 *     *timer - The pointer to ri_timer_t structure.
 *     *name  - The timer name to start.
 *
 * Returns:
 *
 *     None.
 *
 */
void
ri_timer_start(ri_timer_t *timer, const char *name)
{
	timerinfo_t *p;

	ri_log_and_return_if(name == NULL);

	p = (timerinfo_t *)ri_hash_lookup(timer->entrylist, name);

	if (p == NULL) {
		/* there is no entry */
		p = (timerinfo_t *)ri_mem_alloc(sizeof(timerinfo_t));
		p->count   = 0;
		p->elapsed = 0.0;
		p->name = strdup(name);

		set_start_time(p);

		ri_hash_insert(timer->entrylist, name, (void *)p);
	} else {
		set_start_time(p);
	}
}

void
ri_timer_end(ri_timer_t *timer, const char *name)
{
	timerinfo_t *p;

	ri_log_and_return_if(name == NULL);

	p = ri_hash_lookup(timer->entrylist, name);

	if (p == NULL) {
		printf("name = %s\n", name);
		ri_log(LOG_WARN, "timer is not started");
		return;
	} else {
		if (!p->starting) {
			ri_log(LOG_WARN, "timer is not started");
			return;
		}

		set_end_time(p);
		p->count++;
		p->elapsed += calc_elapsed_time(p);
	}	
}

double
ri_timer_elapsed(ri_timer_t *timer, const char *name)
{
	timerinfo_t *p;

	if (name == NULL) return 0.0;

	p = ri_hash_lookup(timer->entrylist, name);

	if (p == NULL) {
		//ri_log("warning", "timer entry not found");
		return 0.0;
	}

	return p->elapsed;
}

double
ri_timer_elapsed_current(ri_timer_t *timer, const char *name)
{
	timerinfo_t *p;
	double       current;

	if (name == NULL) return 0.0;

	p = ri_hash_lookup(timer->entrylist, name);

	if (p == NULL) {
		//ri_log("warning", "timer entry not found");
		return 0.0;
	}

	current = get_elapsed_time_from_start(p);

	return current;
}

void
ri_timer_dump(ri_timer_t *timer)
{
	double total = 0.0;

	printf("\n");
	printf("/= Timing Statistics ==========================================================\n");
	printf("| \n");

	ri_hash_traverse(timer->entrylist, timerinfo_dump_func, NULL);

	total = ri_timer_elapsed(timer, "TOTAL rendering time");

	printf("| -----------------------------------------------------------------------------\n");
	printf("| %-48s:  %20.6f secs\n", "TOTAL rendering time", total);
	printf("| \n");
	printf("\\------------------------------------------------------------------------------\n");
}

/* --- private --- */

void
set_start_time(timerinfo_t *timer)
{
#if defined(WIN32)
	QueryPerformanceFrequency(&timer->freq);
	QueryPerformanceCounter(&timer->start);
#else
	/* unix family */
	gettimeofday(&(timer->start), NULL);
#endif
	timer->starting = 1;

}

void
set_end_time(timerinfo_t *timer)
{
#if defined(WIN32)
	QueryPerformanceCounter(&timer->end);
#else
	gettimeofday(&(timer->end), NULL);
#endif
	timer->starting = 0;
}

double
get_elapsed_time_from_start(timerinfo_t *timer)
{
	double elapsed;
	timerinfo_t tmp;

#if defined(WIN32)
	QueryPerformanceCounter(&(tmp.end));
	elapsed = ((double)tmp.end.QuadPart - (double)timer->start.QuadPart) / (double)timer->freq.QuadPart;
#else
	gettimeofday(&(tmp.end), NULL);
	elapsed = (double)(tmp.end.tv_sec - timer->start.tv_sec) + 
		  (double)(tmp.end.tv_usec - timer->start.tv_usec) / (double)1.0e6;
#endif

	return elapsed;
}

double
calc_elapsed_time(timerinfo_t *timer)
{
	double elapsed;
#if defined(WIN32)
	elapsed = ((double)timer->end.QuadPart - (double)timer->start.QuadPart) / (double)timer->freq.QuadPart;
#else
	elapsed = (double)(timer->end.tv_sec - timer->start.tv_sec) + 
		  (double)(timer->end.tv_usec - timer->start.tv_usec) / 1.0e6;
#endif

	return elapsed;
}

void
timerinfo_free_func(void *data, void *userdata)
{
	timerinfo_t *p;

    (void)userdata;

	assert(data != NULL);

	p = (timerinfo_t *)data;

	if (p->name) free(p->name);

	ri_mem_free(p);
}

void
timerinfo_dump_func(void *data, void *userdata)
{
	timerinfo_t *p;

    (void)userdata;

	assert(data != NULL);

	p = (timerinfo_t *)data;

	if (strcmp(p->name, "TOTAL rendering time") == 0) return;

	printf("| %-48s:  %20.6f secs\n", p->name, p->elapsed);
}

void
timerinfo_sum_func(void *data, void *userdata)
{
	timerinfo_t *p;

	assert(data != NULL);

	p = (timerinfo_t *)data;

	*((double *)userdata) += p->elapsed;
}
